module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Json.Decode as J exposing (Decoder)
import Process     as P
import Url.Builder as UB
import SHA256      as SHA
import Task        as T
import Time

import Element as Element exposing (Element, el, text, image, paragraph, column, row, fill, rgb255)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input
import Element.Lazy       as Lazy

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

main : Program () Model Msg
main = Browser.document { init = init
                        , update = update
                        , subscriptions = subscriptions
                        , view = \model -> { title = "Panic Button", body = [view model] }
                        }

retryDelaySec : Float
retryDelaySec = 15

confirmationInputId : String
confirmationInputId = "confirmationInputTextId"

confirmationTriggerText : String
confirmationTriggerText = "YES"

confirmationTriggered : String -> Bool
confirmationTriggered input = input == confirmationTriggerText

hostStatusOK : String -> Bool
hostStatusOK status = status == "OK"

buttonUrl : String
buttonUrl = UB.relative ["assets", "red-button.png"] []

backgroundColor : Element.Color
backgroundColor = rgb255 100 50 50

fontColor : Element.Color
fontColor = rgb255 200 220 220

type Url  = Url  String
type Host = Host String

fromUrl : Url -> String
fromUrl (Url url) = url

fromHost : Host -> String
fromHost (Host host) = host

type ModelState = Init
                | Ready
                | AwaitConfirm String
                | Locking Progress
                | Done Progress (Maybe Url)

type alias Model = { state  : ModelState
                   , config : Config
                   , log    : List String
                   }

type alias Config = { hosts : List Host
                    , mock  : Bool
                    }

type alias Progress = { total : Int
                      , lockingProgress  : ProgressState
                      , verifyingProgress: ProgressState
                      }

type alias ProgressState = Int

type alias HttpResult res = Result Http.Error res

type Msg = HostConfigMsg (HttpResult (List Host))
         | LockMsg
         | LockDoneMsg Host (HttpResult String)
         | VerifyDoneMsg Host (HttpResult String)
         | RetryMsg Host (Cmd Msg)
         | TagRequestMsg (Time.Posix -> Cmd Msg) Time.Posix
         | FoundGifMsg (HttpResult Url)
         | UpdateConfirmTextMsg String
         | UpdateMockCheckboxMsg Bool
         | FocusedMsg String

printError : Http.Error -> String
printError error = case error of
  Http.BadUrl s     -> "BadUrl: " ++ s
  Http.Timeout      -> "Timeout"
  Http.NetworkError -> "NetworkError"
  Http.BadStatus i  -> "BadStatus: " ++ (String.fromInt i)
  Http.BadBody s    -> "BadBody: " ++ s

appendLog : Model -> String -> Model
appendLog model msg = { model | log = model.log ++ [msg] }

appendLogs : Model -> List String -> Model
appendLogs = List.foldl <| flip appendLog

updateConfig : Model -> (Config -> Config) -> Model
updateConfig model doUpdate = { model | config = doUpdate model.config }

init : () -> (Model, Cmd Msg)
init _ = (initModel, getHostConfig)

initModel : Model
initModel = { state  = Init
            , config = { hosts = [], mock = True }
            , log    = []
            }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  HostConfigMsg hosts          -> (gotHosts model hosts, Cmd.none)
  LockMsg                      -> ({ model | state = AwaitConfirm "" }, tryFocus confirmationInputId)
  LockDoneMsg host result      -> gotLockDone model host result
  VerifyDoneMsg host result    -> gotVerifyDone model host result
  RetryMsg host cmd            -> (appendLog model <| "Retrying: " ++ (fromHost host), cmd)
  TagRequestMsg mkRequest time -> (model, mkRequest time)
  FoundGifMsg result           -> gotGif model result
  UpdateConfirmTextMsg txt     -> gotConfirm txt model
  UpdateMockCheckboxMsg mock   -> (updateConfig model <| \oldConfig -> { oldConfig | mock = mock }, Cmd.none)
  FocusedMsg id                -> (appendLog model <| "Focused element with id=" ++ id, Cmd.none)

gotHosts : Model -> HttpResult (List Host) -> Model
gotHosts model res = case res of
  Ok  hosts -> let addLogs  = flip appendLogs <| "Servers to lock:" :: (List.map fromHost hosts)
                   setHosts = flip updateConfig <| \oldConfig -> { oldConfig | hosts = hosts }
               in addLogs << setHosts <| { model | state = Ready }
  Err err   -> appendLog model << printError <| err

gotConfirm : String -> Model -> (Model, Cmd Msg)
gotConfirm confirmTxt model =
  let setLocking m = { m | state = Locking { total = List.length m.config.hosts
                                           , lockingProgress   = 0
                                           , verifyingProgress = 0
                                           }
                     }
      addLog = flip appendLog "Locking the servers..."
  in if confirmationTriggered confirmTxt
     then (addLog << setLocking <| model, doLock model.config)
     else ({ model | state = AwaitConfirm confirmTxt }, Cmd.none)

doLock : Config -> Cmd Msg
doLock cfg = Cmd.batch << List.map (flip lockServer cfg.mock) <| cfg.hosts

gotLockDone : Model -> Host -> HttpResult String -> (Model, Cmd Msg)
gotLockDone model host res =
  let mkNewCmd _ _ = verifyServer host model.config.mock
      retryCmd     = lockServer   host model.config.mock
      increaseLockingCount progress = { progress | lockingProgress = progress.lockingProgress + 1 }
  in gotLockingProgress model host res retryCmd "Lock" increaseLockingCount mkNewCmd

gotVerifyDone : Model -> Host -> HttpResult String -> (Model, Cmd Msg)
gotVerifyDone model host res =
  let mkNewCmd _ = verifyDoneCmd
      retryCmd   = verifyServer host model.config.mock
      increaseVerifyingCount progress = { progress | verifyingProgress = progress.verifyingProgress + 1 }
  in gotLockingProgress model host res retryCmd "Verify" increaseVerifyingCount mkNewCmd

gotLockingProgress : Model ->
                     Host ->
                     HttpResult String ->
                     Cmd Msg ->
                     String ->
                     (Progress -> Progress) ->
                     (Model -> Progress -> Cmd Msg) ->
                     (Model, Cmd Msg)
gotLockingProgress model host result retryCmd logHeader incrProgress mkNewCmd =
  let doLog = formatProgressMsg host logHeader
      doRetry = retry host retryCmd
      ifOK progress = newProgressModel model progress doLog incrProgress mkNewCmd
      ifNOK = (appendLog model <| doLog "unsuccessful", doRetry)
  in case result of
    Ok  hostStatus -> if hostStatusOK hostStatus
                      then assumeLocking model ifOK
                      else ifNOK
    Err err        -> progressError model doRetry err doLog

assumeLocking : Model -> (Progress -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
assumeLocking model mkNewModel = case model.state of
  Locking progress -> mkNewModel progress
  _                -> (appendLog model "Model in unexpected state, ignoring...", Cmd.none)

newProgressModel : Model ->
                   Progress ->
                   (String -> String) ->
                   (Progress -> Progress) ->
                   (Model -> Progress -> Cmd Msg) ->
                   (Model, Cmd Msg)
newProgressModel model progress doLog incr mkNewCmd =
  let newProgress = incr progress
      newModel    = { model | state = Locking newProgress }
  in (appendLog newModel <| doLog "success", mkNewCmd newModel newProgress)

progressError : Model -> Cmd Msg -> Http.Error -> (String -> String) -> (Model, Cmd Msg)
progressError model retryCmd err doLog =
  (appendLog model << doLog << printError <| err, retryCmd)

formatProgressMsg : Host -> String -> String -> String
formatProgressMsg host logHeader msg = logHeader ++ ": " ++ (fromHost host) ++ ": " ++ msg

verifyDoneCmd : Progress -> Cmd Msg
verifyDoneCmd progress = if progress.verifyingProgress == progress.total
                         then getRandomCatGif
                         else Cmd.none

retry : Host -> Cmd Msg -> Cmd Msg
retry host cmd = T.perform (\_ -> RetryMsg host cmd) << P.sleep <| retryDelaySec * 1000

tryFocus : String -> Cmd Msg
tryFocus id = T.attempt (\_ -> FocusedMsg id) <| Dom.focus id

gotGif : Model -> HttpResult Url -> (Model, Cmd Msg)
gotGif model res = assumeLocking model (\progress ->
  let newModel = case res of
                   Ok  url ->           { model | state = Done progress (Maybe.Just url) }
                   Err err -> appendLog { model | state = Done progress Maybe.Nothing } << printError <| err
  in (appendLog newModel "Reached final state.", Cmd.none))

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model = Element.layout [ Background.color backgroundColor
                            , Font.color fontColor
                            , Element.width fill
                            , Element.height fill
                            , Element.scrollbars
                            , Element.padding 15
                            ] <| viewElement model

viewElement : Model -> Element Msg
viewElement model =
  el [ Element.width fill
     , Element.height fill
     , printLog model
     ] <|
     case model.state of
       Init              -> el [ Font.size 30 ] <| text "Loading the app config..."
       Ready             -> viewReady model
       AwaitConfirm txt  -> viewConfirm model txt
       Locking progress  -> viewProgress model progress Maybe.Nothing
       Done progress url -> viewProgress model progress url

viewReady : Model -> Element Msg
viewReady model =
  column [ Element.width fill
         , Element.height fill
         ]
         [ el [ Element.centerX ] <| paragraph [ Font.size 50 ] [ text "Secure the servers in your project" ]
         , el [ Element.centerX
              , Element.centerY
              ]
              <| button "Lock the servers" buttonUrl LockMsg
         ]

viewConfirm : Model -> String -> Element Msg
viewConfirm model txt =
  let title = "Confirmation required"
      txtLabel = "Please type " ++ confirmationTriggerText ++ " below to confirm"
      mockLabel = "Do a test run without actually locking the servers, only uncheck this in a real emergency."
      textInput = Input.text [ Font.color (rgb255 0 0 0)
                             , Element.htmlAttribute << HA.id <| confirmationInputId
                             , Element.spacing 15
                             ]
                             { onChange = UpdateConfirmTextMsg
                             , text = txt
                             , placeholder = Maybe.Nothing
                             , label = Input.labelAbove [] (paragraph [] [ text txtLabel ])
                             }
      mockCheckbox = Input.checkbox [ Element.spacing 15 ]
                                    { onChange = UpdateMockCheckboxMsg
                                    , icon = Input.defaultCheckbox
                                    , checked = model.config.mock
                                    , label = Input.labelRight [ Element.width fill ] <| paragraph [] [ text mockLabel ]
                                    }
  in column [ Element.width fill
            , Element.height fill
            ]
            [ el [ Element.centerX ] <| paragraph [ Font.size 50 ] [ text title ]
            , column [ Element.centerX
                     , Element.centerY
                     , Element.spacing 15
                     , Font.size 20
                     ]
                     [ mockCheckbox
                     , textInput
                     ]
            ]

viewProgress : Model -> Progress -> Maybe Url -> Element Msg
viewProgress model progress maybeUrl =
  let mockParagraph = if model.config.mock
                      then paragraph [] [ text "(Beware: you selected "
                                        , el [ Font.bold ] <| text "test mode"
                                        , text ", no servers have actually been disabled!)" ]
                      else Element.none
  in column [ Element.width fill
            , Element.height fill
            , Font.size 20
            ]
            [ column [ Element.centerX
                     , Element.centerY
                     , Element.spacing 10
                     ]
                     [ column [] [ paragraph [] [ text <| "Locking "   ++ (String.fromInt progress.total) ++ " servers." ]
                                 , paragraph [] [ text <| "Locked: "   ++ (printProgress progress .lockingProgress .total) ]
                                 , paragraph [] [ text <| "Verified: " ++ (printProgress progress .verifyingProgress .total) ]
                                 , mockParagraph
                                 ]
                     , Maybe.withDefault Element.none << Maybe.map renderImg <| maybeUrl
                     ]
            ]

printProgress : Progress -> (Progress -> Int) -> (Progress -> Int) -> String
printProgress progress getCount getTotal =
  (String.fromInt << getCount <| progress) ++ "/" ++ (String.fromInt << getTotal <| progress)

renderImg : Url -> Element Msg
renderImg (Url url) = column [ Element.width fill
                             , Element.spacing 5 ]
                             [ paragraph [] [ text "We are done, have a cat." ]
                             , image [ Element.centerX ]
                                     { src = url, description = "Funny cat, we're done!" }
                             ]

printLog : Model -> Element.Attribute Msg
printLog model = Element.behindContent << Lazy.lazy doPrintLog <| model.log

doPrintLog : List String -> Element Msg
doPrintLog msgs =
  let formatLine msg = paragraph [] [ text msg ]
      logLines = List.map formatLine <| "Debug log:" :: msgs
  in el [ Element.height fill
        , Element.width fill
        , Font.size 10
        ]
        <| column [Element.alignBottom] logLines

button : String -> String -> Msg -> Element Msg
button label src onPress =
  image [ Events.onClick onPress
        , Font.color       (rgb255 255 255 255)
        , Element.focused [ Background.color (rgb255 200 100 100) ]
        ]
        { src = src, description = label }

getHostConfig : Cmd Msg
getHostConfig = Http.get { url = UB.relative ["static", "api", "config"] []
                         , expect = Http.expectJson HostConfigMsg hostsDecoder
                         }

hostsDecoder : Decoder (List Host)
hostsDecoder = J.field "servers" << J.list << J.map Host <| J.string

-- Tag a request with a key based off the current time
-- See the paramFromTime function
tagRequest : (Time.Posix -> Cmd Msg) -> Cmd Msg
tagRequest mkRequest = T.perform (TagRequestMsg mkRequest) Time.now

lockServer : Host -> Bool -> Cmd Msg
lockServer host mock = tagRequest <| doLockServer host mock

doLockServer : Host -> Bool -> Time.Posix -> Cmd Msg
doLockServer host mock time = doLockGet host mock time ["lock"] (LockDoneMsg host)

verifyServer : Host -> Bool -> Cmd Msg
verifyServer host mock = tagRequest <| doVerifyServer host mock

doVerifyServer : Host -> Bool -> Time.Posix -> Cmd Msg
doVerifyServer host mock time = doLockGet host mock time ["verify"] (VerifyDoneMsg host)

-- TODO: only for the testing API from the demo. Replace with doLockPost
doLockGet : Host -> Bool -> Time.Posix -> List String -> (HttpResult String -> Msg) -> Cmd Msg
doLockGet host mock time path mkMsg =
  let queryString = [ UB.string "host" <| fromHost host
                    , paramFromBool "mock" mock
                    , paramFromTime "key" time
                    ]
      url = UB.relative ([ "static", "api" ] ++ path) queryString
  in Http.get { url = url
              , expect = Http.expectJson mkMsg statusDecoder
              }

doLockPost : Host -> Bool -> Time.Posix -> List String -> (HttpResult String -> Msg) -> Cmd Msg
doLockPost host mock time path mkMsg =
  let url = UB.crossOrigin ("https://" ++ (fromHost host))
                           ("api" :: path)
                           [ paramFromBool "mock" mock
                           , paramFromTime "key" time
                           ]
  in Http.post { url = url
               , body = Http.emptyBody
               , expect = Http.expectJson mkMsg statusDecoder
               }

paramFromBool : String -> Bool -> UB.QueryParameter
paramFromBool name value = UB.string name <| if value then "true" else "false"

-- Tag a request with a key based off the current time which can be validated in the backend.
-- We divide by 2000 to have windows with a width of 2 seconds in which a request can be send,
-- and we can accept requests with a key corresponding to the previous window to avoid
-- edge cases when a request is sent right before a new window starts.
-- This validation of the key will serve as a basic protection against curious employees.
paramFromTime : String -> Time.Posix -> UB.QueryParameter
paramFromTime name time =
  UB.string name
    << SHA.toHex
    << SHA.fromString
    << String.fromInt
    <| Time.posixToMillis time // 2000

statusDecoder : Decoder String
statusDecoder = J.field "status" J.string

getRandomCatGif : Cmd Msg
getRandomCatGif = Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
                           , expect = Http.expectJson FoundGifMsg gifDecoder
                           }

gifDecoder : Decoder Url
gifDecoder = J.field "data" << J.field "image_url" << J.map Url <| J.string


