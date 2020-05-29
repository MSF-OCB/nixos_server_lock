module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Json.Decode as J exposing (Decoder)
import Process     as P
import Url.Builder as UB
import Task        as T

import Element as Element exposing (Element, el, text, image, column, row, fill, rgb255)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

main = Browser.element { init = init
                       , update = update
                       , subscriptions = subscriptions
                       , view = view
                       }

retryDelaySec : Float
retryDelaySec = 15

buttonUrl : String
buttonUrl = UB.relative ["static", "red-button.png"] []

backgroundColor : Element.Color
backgroundColor = rgb255 100 50 50

fontColor : Element.Color
fontColor = rgb255 200 220 220

confirmationInputId : String
confirmationInputId = "confirmationInputTextId"

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
         | ConfirmMsg
         | LockMsg
         | LockDoneMsg Host (HttpResult String)
         | VerifyDoneMsg Host (HttpResult String)
         | Retry Host (Cmd Msg)
         | FoundGifMsg (HttpResult Url)
         | UpdateConfirmText String
         | MockCheckboxChanged Bool
         | Focused String

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
appendLogs = List.foldl (\s m -> appendLog m s)

updateConfig : Model -> (Config -> Config) -> Model
updateConfig model updateC =
  let oldConfig = model.config
  in { model | config = updateC oldConfig }

init : () -> (Model, Cmd Msg)
init _ = (initModel, getHostConfig)

initModel : Model
initModel = { state  = Init
            , config = { hosts = [], mock = True }
            , log    = []
            }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  HostConfigMsg hosts       -> (gotConfig model hosts, Cmd.none)
  LockMsg                   -> ({ model | state = AwaitConfirm "" }, tryFocus confirmationInputId)
  ConfirmMsg                -> (gotConfirm model, doLock model.config)
  LockDoneMsg host result   -> gotLockDone model host result
  VerifyDoneMsg host result -> gotVerifyDone model host result
  Retry host cmd            -> (appendLog model << (++) "Retrying: " << fromHost <| host, cmd)
  FoundGifMsg result        -> gotGif model result
  UpdateConfirmText txt     -> case model.state of
    AwaitConfirm _ -> ({ model | state = AwaitConfirm txt }, Cmd.none)
    _              -> (model, Cmd.none)
  MockCheckboxChanged mock  -> (updateConfig model (\oldConfig -> { oldConfig | mock = mock }), Cmd.none)
  Focused id                -> (appendLog model <| "Focused element with id=" ++ id, Cmd.none)

gotConfig : Model -> HttpResult (List Host) -> Model
gotConfig model res = case res of
  Ok  hosts -> let doLogs   = flip appendLogs ("Servers to lock:" :: (List.map fromHost hosts))
                   doConfig = flip updateConfig (\oldConfig -> { oldConfig | hosts = hosts })
               in doLogs << doConfig <| { model | state = Ready }
  Err err   -> appendLog model << printError <| err

gotConfirm : Model -> Model
gotConfirm model = appendLog { model | state = Locking { total = List.length model.config.hosts
                                                       , lockingProgress   = 0
                                                       , verifyingProgress = 0
                                                       }
                             }
                             "Locking servers..."

doLock : Config -> Cmd Msg
doLock cfg = Cmd.batch << List.map (lockServer cfg.mock) <| cfg.hosts

gotLockDone : Model -> Host -> HttpResult String -> (Model, Cmd Msg)
gotLockDone model host res =
  let newCmd _ _ = verifyServer model.config.mock host
      retryCmd   = lockServer model.config.mock host
      increaseLockingCount progress = { progress | lockingProgress = progress.lockingProgress + 1 }
  in gotLockingProgress model host res "Lock" retryCmd increaseLockingCount newCmd

gotVerifyDone : Model -> Host -> HttpResult String -> (Model, Cmd Msg)
gotVerifyDone model host res =
  let newCmd _ = verifyDoneCmd
      retryCmd = verifyServer model.config.mock host
      increaseVerifyingCount progress = { progress | verifyingProgress = progress.verifyingProgress + 1 }
  in gotLockingProgress model host res "Verify" retryCmd increaseVerifyingCount newCmd

gotLockingProgress : Model ->
                     Host ->
                     HttpResult String ->
                     String ->
                     Cmd Msg ->
                     (Progress -> Progress) ->
                     (Model -> Progress -> Cmd Msg) ->
                     (Model, Cmd Msg)
gotLockingProgress model host result logHeader retryCmd incrProgress newCmd =
  let doRetry = retry host retryCmd
  in case result of
    Ok  host_status -> if host_status == "OK"
                       then assumeLocking model (\progress ->
                         let (newModel, newProgress) = newProgressModel model progress host logHeader incrProgress
                         in (newModel, newCmd newModel newProgress)
                       )
                       else (appendLog model << formatProgressMsg host logHeader <| "unsuccessful", doRetry)
    Err err         -> progressError model host logHeader doRetry err

assumeLocking : Model -> (Progress -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
assumeLocking model fun = case model.state of
  Locking progress -> fun progress
  _                -> (appendLog model "Model in unexpected state, ignoring...", Cmd.none)

newProgressModel : Model -> Progress -> Host -> String -> (Progress -> Progress) -> (Model, Progress)
newProgressModel model progress host logHeader incr =
  let newProgress = incr progress
      newModel    = { model | state = Locking newProgress }
  in (appendLog newModel << formatProgressMsg host logHeader <| "success", newProgress)

progressError : Model -> Host -> String -> Cmd Msg -> Http.Error -> (Model, Cmd Msg)
progressError model host logHeader retryCmd err =
  (appendLog model << formatProgressMsg host logHeader << printError <| err, retryCmd)

formatProgressMsg : Host -> String -> String -> String
formatProgressMsg host logHeader msg = logHeader ++ ": " ++ (fromHost host) ++ ": " ++ msg

verifyDoneCmd : Progress -> Cmd Msg
verifyDoneCmd progress = if progress.verifyingProgress == progress.total
                         then getRandomCatGif
                         else Cmd.none

retry : Host -> Cmd Msg -> Cmd Msg
retry host cmd = T.perform (\_ -> Retry host cmd) << P.sleep <| (retryDelaySec * 1000)

tryFocus : String -> Cmd Msg
tryFocus id = T.attempt (\_ -> Focused id) (Dom.focus id)

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
                            , Font.size 50
                            , Element.width fill
                            , Element.height fill
                            , Element.padding 15
                            ] (viewElement model)

viewElement : Model -> Element Msg
viewElement model =
  el [ Element.width fill
     , Element.height fill
     , Element.scrollbars
     , printLog model
     ]
     ( case model.state of
         Init              -> el [Font.size 30] ( text "Loading the app config..." )
         Ready             -> viewReady model
         AwaitConfirm txt  -> viewConfirm model txt
         Locking progress  -> viewProgress model progress Maybe.Nothing
         Done progress url -> viewProgress model progress url
     )

viewReady : Model -> Element Msg
viewReady model =
  column [ Element.width fill
         , Element.height fill
         ]
         [ el [ Element.centerX ]
              ( text "Secure the servers in your project" )
         , el [ Element.centerX
              , Element.centerY
              ]
              ( button "Lock the servers" buttonUrl LockMsg )
         ]

viewConfirm : Model -> String -> Element Msg
viewConfirm model txt =
  let title = "Confirmation required"
      confirmText = "YES"
      txtLabel = "Please type " ++ confirmText ++ " below to confirm"
      mockLabel = "Uncheck to disable test mode"
      textInput = Input.text [ Font.color (rgb255 0 0 0)
                             , Element.htmlAttribute << HA.id <| confirmationInputId
                             , Element.spacing 15
                             ]
                             { onChange = \s -> if s == confirmText
                                                then ConfirmMsg
                                                else UpdateConfirmText s
                             , text = txt
                             , placeholder = Maybe.Nothing
                             , label = Input.labelAbove [] (text txtLabel)
                             }
      mockCheckbox = Input.checkbox [ Element.spacing 15 ]
                                    { onChange = MockCheckboxChanged
                                    , icon = Input.defaultCheckbox
                                    , checked = model.config.mock
                                    , label = Input.labelRight [] (text mockLabel)
                                    }
  in column [ Element.width fill
            , Element.height fill
            ]
            [ el [ Element.centerX ]
                 ( text title )
            , column [ Element.centerX
                     , Element.centerY
                     , Element.spacing 15
                     , Font.size 20
                     ]
                     [ textInput
                     , mockCheckbox
                     ]
            ]

viewProgress : Model -> Progress -> Maybe Url -> Element Msg
viewProgress model progress maybeUrl =
  column [ Element.width fill
         , Element.height fill
         , Font.size 20
         ]
         [ column [ Element.centerX
                  , Element.centerY
                  , Element.spacing 10
                  ]
                  [ column [] [ text ("Locking " ++ (String.fromInt progress.total) ++ " servers.")
                              , text ("Locked: " ++ (String.fromInt progress.lockingProgress) ++ "/" ++ (String.fromInt progress.total))
                              , text ("Verified: " ++ (String.fromInt progress.verifyingProgress) ++ "/" ++ (String.fromInt progress.total))
                              ]
                  , if model.config.mock
                    then text ("(Beware: you selected test mode,\n no servers have actually been disabled!)")
                    else Element.none
                  , Maybe.withDefault Element.none << Maybe.map renderImg <| maybeUrl
                  ]
         ]

renderImg : Url -> Element Msg
renderImg (Url url) = column [ Element.centerX
                             , Element.spacing 5
                             ]
                             [ text "We are done, have a cat."
                             , image [Element.centerX]
                                     { src = url, description = "Funny cat, we're done!" }
                             ]

printLog : Model -> Element.Attribute Msg
printLog model =
  Element.behindContent (el [ Element.height fill
                            , Element.width fill
                            , Font.size 10
                            ]
                            ( column [Element.alignBottom]
                                     [ text "Debug log:\n"
                                     , text << String.join "\n" <| model.log
                                     ]
                            )
                        )

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

lockServer : Bool -> Host -> Cmd Msg
lockServer mock host = Http.get { url = UB.relative ["static", "api", "lock"] [ UB.string "host" (fromHost host)
                                                                              , paramFromBool "mock" mock
                                                                              ]
                                  --url = UB.crossOrigin ("https://" ++ name) ["api", "lock"] []
                                --, body = Http.emptyBody
                                , expect = Http.expectJson (LockDoneMsg host) statusDecoder
                                }

verifyServer : Bool -> Host -> Cmd Msg
verifyServer mock host = Http.get { url = UB.relative ["static", "api", "verify"] [ UB.string "host" (fromHost host)
                                                                                  , paramFromBool "mock" mock
                                                                                  ]
                                    --url = UB.crossOrigin ("https://" ++ name) ["api", "lock"] []
                                  --, body = Http.emptyBody
                                  , expect = Http.expectJson (VerifyDoneMsg host) statusDecoder
                                  }

paramFromBool : String -> Bool -> UB.QueryParameter
paramFromBool name value = UB.string name (if value then "true" else "false")

statusDecoder : Decoder String
statusDecoder = J.field "status" <| J.string

getRandomCatGif : Cmd Msg
getRandomCatGif = Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
                           , expect = Http.expectJson FoundGifMsg gifDecoder
                           }

gifDecoder : Decoder Url
gifDecoder = J.field "data" << J.field "image_url" << J.map Url <| J.string


