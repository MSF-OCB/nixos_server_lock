module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as J exposing (Decoder)
import Process     as P
import Url.Builder as UB
import SHA256      as SHA
import Task        as T
import Time

import Element as Element exposing (Element
                                   , el, text, image, paragraph, column, row, wrappedRow, newTabLink
                                   , alignBottom, width, height, fill, centerX, centerY, padding, spacing
                                   , Color, rgb255
                                   )
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input
import Element.Lazy       as Lazy

--
-- TODO items
--
-- 1. Implement a maximum for the number of retries.
--      -> this might require maintaining a dict in the model containing info per host
-- 2. Message to inform the user that everything is done
-- 3. Detailed error messages, this may require a dict like mentioned in item 1
--

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

black : Color
black = rgb255 0 0 0

grey : Color
grey = rgb255 234 237 243

darkGrey : Color
darkGrey = rgb255 224 227 233

white : Color
white = rgb255 255 255 255

red : Color
red = rgb255 238 0 0

backgroundColor : Color
backgroundColor = white

buttonBackgroundColor : Color
buttonBackgroundColor = white

fontColor : Color
fontColor = black

msfLogoPath : Path
msfLogoPath = [ "assets", "azg-logo.svg" ]

type Url  = Url  String
type Host = Host String
type alias Path = List String

fromUrl : Url -> String
fromUrl (Url url) = url

fromHost : Host -> String
fromHost (Host host) = host

type ModelState = Init
                | AwaitConfirm String
                | Locking Progress

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
         | ConfirmMsg
         | LockDoneMsg Host (HttpResult String)
         | VerifyDoneMsg Host (HttpResult String)
         | RetryMsg Host (Cmd Msg)
         | TagRequestMsg (Time.Posix -> Cmd Msg) Time.Posix
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
  HostConfigMsg hosts          -> (gotHosts model hosts, tryFocus confirmationInputId)
  ConfirmMsg                   -> gotConfirm model
  LockDoneMsg host result      -> gotLockDone model host result
  VerifyDoneMsg host result    -> gotVerifyDone model host result
  RetryMsg host cmd            -> (appendLog model <| "Retrying: " ++ (fromHost host), cmd)
  TagRequestMsg mkRequest time -> (model, mkRequest time)
  UpdateConfirmTextMsg txt     -> ({ model | state = AwaitConfirm txt }, Cmd.none)
  UpdateMockCheckboxMsg mock   -> (updateConfig model <| \oldConfig -> { oldConfig | mock = mock }, Cmd.none)
  FocusedMsg id                -> (appendLog model <| "Focused element with id=" ++ id, Cmd.none)

gotHosts : Model -> HttpResult (List Host) -> Model
gotHosts model res = case res of
  Ok  hosts -> let addLogs  = flip appendLogs <| "Servers to lock:" :: (List.map fromHost hosts)
                   setHosts = flip updateConfig <| \oldConfig -> { oldConfig | hosts = hosts }
               in addLogs << setHosts <| { model | state = AwaitConfirm "" }
  Err err   -> appendLog model << printError <| err

gotConfirm : Model -> (Model, Cmd Msg)
gotConfirm model =
  let setLocking m = { m | state = Locking { total = List.length m.config.hosts
                                           , lockingProgress   = 0
                                           , verifyingProgress = 0
                                           }
                     }
      addLog = flip appendLog "Locking the servers..."
  in (addLog << setLocking <| model, doLock model.config)

doLock : Config -> Cmd Msg
doLock cfg = Cmd.batch << List.map (flip lockServer cfg.mock) <| cfg.hosts

gotLockDone : Model -> Host -> HttpResult String -> (Model, Cmd Msg)
gotLockDone model host res =
  let newCmd   = verifyServer host model.config.mock
      retryCmd = lockServer   host model.config.mock
      increaseLockingCount progress = { progress | lockingProgress = progress.lockingProgress + 1 }
  in gotLockingProgress model host res retryCmd "Lock" increaseLockingCount newCmd

gotVerifyDone : Model -> Host -> HttpResult String -> (Model, Cmd Msg)
gotVerifyDone model host res =
  let newCmd   = Cmd.none
      retryCmd = verifyServer host model.config.mock
      increaseVerifyingCount progress = { progress | verifyingProgress = progress.verifyingProgress + 1 }
  in gotLockingProgress model host res retryCmd "Verify" increaseVerifyingCount newCmd

gotLockingProgress : Model ->
                     Host ->
                     HttpResult String ->
                     Cmd Msg ->
                     String ->
                     (Progress -> Progress) ->
                     Cmd Msg ->
                     (Model, Cmd Msg)
gotLockingProgress model host result retryCmd logHeader incrProgress newCmd =
  let doLog = formatProgressMsg host logHeader
      doRetry = retry host retryCmd
      ifOK progress = newProgressModel model progress doLog incrProgress newCmd
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
                   Cmd Msg ->
                   (Model, Cmd Msg)
newProgressModel model progress doLog incr newCmd =
  let newModel = { model | state = Locking <| incr progress }
  in (appendLog newModel <| doLog "success", newCmd)

progressError : Model -> Cmd Msg -> Http.Error -> (String -> String) -> (Model, Cmd Msg)
progressError model retryCmd err doLog =
  (appendLog model << doLog << printError <| err, retryCmd)

formatProgressMsg : Host -> String -> String -> String
formatProgressMsg host logHeader msg = logHeader ++ ": " ++ (fromHost host) ++ ": " ++ msg

retry : Host -> Cmd Msg -> Cmd Msg
retry host cmd = T.perform (\_ -> RetryMsg host cmd) << P.sleep <| retryDelaySec * 1000

tryFocus : String -> Cmd Msg
tryFocus id = T.attempt (\_ -> FocusedMsg id) <| Dom.focus id

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
  let focusStyle = { borderColor = Just black
                   , backgroundColor = Nothing
                   , shadow = Nothing
                   }
      content = viewElement model
  in Element.layoutWith { options = [ Element.focusStyle focusStyle ] }
                        [ Background.color backgroundColor
                        , Font.size 20
                        , Font.color fontColor
                        , width fill
                        , height fill
                        , Element.scrollbars
                        , padding 15
                        ]
                        content

viewElement : Model -> Element Msg
viewElement model =
  let title = wrappedRow [ width fill
                         , Font.size 50
                         , Font.center
                         ]
                         [ el [ width fill ] <| text "MSF server "
                         , el [ width fill, Font.color red ] <| text "panic button"
                         ]
      titleElement = el [ centerX
                        , centerY
                        ]
                        <| title
      mainElement = el [ height fill
                       , width fill
                       ]
                       <| case model.state of
                            Init             -> el [] <| text "Loading the app config..."
                            AwaitConfirm txt -> viewConfirm model txt
                            Locking progress -> viewProgress model progress
      msfImage = image [ height << Element.minimum 50 <| fill
                       , width fill
                       , alignBottom
                       ]
                       { src = UB.relative msfLogoPath []
                       , description = "MSF logo"
                       }
      msfLogo = el [ alignBottom
                   , width fill
                   , padding 10
                   ]
                   <| newTabLink [ centerX ]
                                 { url = "https://www.msf-azg.be"
                                 , label = msfImage
                                 }
  in column [ width fill
            , height fill
            , printLog model
            ]
            [ titleElement
            , mainElement
            , msfLogo
            ]

viewConfirm : Model -> String -> Element Msg
viewConfirm model txt =
  let ifConfirmed yes no = if confirmationTriggered txt then yes else no
      goAction = ifConfirmed (Just ConfirmMsg) Nothing
      textLabel = paragraph [] [ text "Please type "
                               , el [ Font.bold, Font.color red ] <| text confirmationTriggerText
                               , text " below to confirm"
                               ]
      mockDescription = [ "Do a test run without actually locking the servers.", "Only uncheck this in a real emergency." ]
      mockLabel = "Test mode"
      mockCheckbox = Input.checkbox [ spacing 15 ]
                                    { onChange = UpdateMockCheckboxMsg
                                    , icon = Input.defaultCheckbox
                                    , checked = model.config.mock
                                    , label = Input.labelRight [ width fill ] <| paragraph [] [ text mockLabel ]
                                    }
      textInput = Input.text [ Element.htmlAttribute << HA.id <| confirmationInputId
                             , spacing 15
                             , onEnter goAction
                             ]
                             { onChange = UpdateConfirmTextMsg
                             , text = txt
                             , placeholder = Just << Input.placeholder [] << paragraph [] <| [ text confirmationTriggerText ]
                             , label = Input.labelAbove [] textLabel
                             }
      goButton = Input.button [ Border.width 1
                              , Border.solid
                              , Border.rounded 3
                              , ifConfirmed Font.bold Font.regular
                              , Font.color <| ifConfirmed red grey
                              , Border.color <| ifConfirmed black grey
                              , Element.mouseOver [ Background.color <| ifConfirmed darkGrey white ]
                              ]
                              { onPress = ifConfirmed (Just ConfirmMsg) Nothing
                              , label = paragraph [ padding 5 ] [ text "Go!" ]
                              }
  in column [ width fill
            , height fill
            ]
            [ column [ centerX
                     , centerY
                     , spacing 15
                     ]
                     [ paragraph [] [ text "<warning about the consequences>" ]
                     , column [ Border.width 1
                              , Border.solid
                              , Border.rounded 3
                              , Border.color black
                              , centerX
                              , width fill
                              , padding 20
                              , spacing 15
                              ]
                              [ mockCheckbox
                              , textInput
                              , goButton
                              ]
                     ]
            ]

onEnter : Maybe msg -> Element.Attribute msg
onEnter = Element.htmlAttribute
  << HE.on "keyup"
  << Maybe.withDefault (J.fail "No message")
  << Maybe.map (\msg -> J.andThen (\key -> if key == "Enter"
                                           then J.succeed msg
                                           else J.fail "Not the enter key")
                        <| J.field "key" J.string)

viewProgress : Model -> Progress -> Element Msg
viewProgress model progress =
  let mockParagraph = if model.config.mock
                      then paragraph [] [ text "Beware: you selected "
                                        , el [ Font.bold, Font.color red ] <| text "test mode"
                                        , text ", no servers will actually be disabled!" ]
                      else Element.none
  in column [ width fill
            , height fill
            ]
            [ column [ centerX
                     , centerY
                     , spacing 15
                     ]
                     [ mockParagraph
                     , column [] [ paragraph [] [ text <| "Locking "   ++ (String.fromInt progress.total) ++ " servers." ]
                                 , paragraph [] [ text <| "Locked: "   ++ (printProgress progress .lockingProgress .total) ]
                                 , paragraph [] [ text <| "Verified: " ++ (printProgress progress .verifyingProgress .total) ]
                                 ]
                     ]
            ]

printProgress : Progress -> (Progress -> Int) -> (Progress -> Int) -> String
printProgress progress getCount getTotal =
  (String.fromInt << getCount <| progress) ++ "/" ++ (String.fromInt << getTotal <| progress)

printLog : Model -> Element.Attribute Msg
printLog model = Element.behindContent << Lazy.lazy doPrintLog <| model.log

doPrintLog : List String -> Element Msg
doPrintLog msgs =
  let formatLine msg = paragraph [] [ text msg ]
      logLines = List.map formatLine <| "Debug log:" :: msgs
  in el [ height fill
        , width fill
        , Font.size 3
        ]
        <| column [ alignBottom ] logLines

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
doLockGet : Host -> Bool -> Time.Posix -> Path -> (HttpResult String -> Msg) -> Cmd Msg
doLockGet host mock time path mkMsg =
  let queryString = [ UB.string "host" <| fromHost host
                    , paramFromBool "mock" mock
                    , paramFromTime "key" time
                    ]
      url = UB.relative ([ "static", "api" ] ++ path) queryString
  in Http.get { url = url
              , expect = Http.expectJson mkMsg statusDecoder
              }

doLockPost : Host -> Bool -> Time.Posix -> Path -> (HttpResult String -> Msg) -> Cmd Msg
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


