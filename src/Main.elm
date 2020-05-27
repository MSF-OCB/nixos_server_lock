module Main exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as J exposing (Decoder)
import Process as P
import Url.Builder as UB
import Task as T

import Element as Element exposing (Element, el, text, image, column, row, fill, rgb255)
import Element.Background as Background
import Element.Border     as Border
import Element.Events     as Events
import Element.Font       as Font
import Element.Input      as Input

main = Browser.element { init = init
                       , update = update
                       , subscriptions = subscriptions
                       , view = view
                       }

type Url  = Url  String
type Host = Host String

fromUrl : Url -> String
fromUrl (Url url) = url

fromHost : Host -> String
fromHost (Host host) = host

type ModelState = Init
                | Ready Config
                | AwaitConfirm Config String
                | Restarting RestartedState
                | Done RestartedState (Maybe Url)

type alias Model = { state : ModelState
                   , log   : List String
                   }

type alias Config = { hosts : List Host }

type alias RestartedState = { total : Int
                            , count : Int
                            }

type alias HttpResult res = Result Http.Error res

type Msg = ConfigMsg (HttpResult Config)
         | ConfirmMsg Config
         | RestartMsg Config
         | RestartDoneMsg Host (HttpResult String)
         | Retry Host
         | FoundGifMsg (HttpResult Url)
         | UpdateConfirmText String

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
appendLogs model msgs = List.foldl (\s m -> appendLog m s) model msgs

init : () -> (Model, Cmd Msg)
init _ = (initModel, getConfig)

initModel : Model
initModel = { state = Init, log = [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  ConfigMsg result           -> (gotConfig model result, Cmd.none)
  RestartMsg cfg             -> ({ model | state = AwaitConfirm cfg "" }, Cmd.none)
  ConfirmMsg cfg             -> (gotConfirm model cfg, doRestart cfg)
  RestartDoneMsg host result -> gotRestartDone model host result
  Retry host                 -> (appendLog model <| "Retrying: " ++ (fromHost host), restartServer host)
  FoundGifMsg result         -> (gotGif model result, Cmd.none)
  UpdateConfirmText txt      -> case model.state of
    AwaitConfirm cfg _ -> ( { model | state = AwaitConfirm cfg txt }, Cmd.none)
    _                  -> (model, Cmd.none)

gotConfig : Model -> (Result Http.Error Config) -> Model
gotConfig model res = case res of
  Ok  cfg -> appendLogs { model | state = Ready cfg }
                        ("Servers to disable:" :: (List.map (fromHost) cfg.hosts))
  Err err -> appendLog model (printError err)

gotConfirm : Model -> Config -> Model
gotConfirm model cfg = appendLog { model | state = Restarting { total = List.length cfg.hosts, count = 0 } }
                                 ("Restarting servers...")

doRestart : Config -> Cmd Msg
doRestart cfg = Cmd.batch << (List.map restartServer) <| cfg.hosts

gotRestartDone : Model -> Host -> (HttpResult String) -> (Model, Cmd Msg)
gotRestartDone model host res = case res of
  Ok  host_status -> case model.state of
    Restarting state -> let newModel = appendLog { model | state = Restarting { state | count = state.count + 1 } }
                                                 host_status
                        in (newModel, restartDoneCmd newModel)
    _                -> (appendLog model "Model in unexpected state, ignoring...", Cmd.none)
  Err err  -> (appendLog model << (\msg -> (fromHost host) ++ ": " ++ msg) << printError <| err, retry host)

retry : Host -> Cmd Msg
retry host = T.perform (\_ -> Retry host) << P.sleep <| (30 * 1000)

restartDoneCmd : Model -> Cmd Msg
restartDoneCmd model = case model.state of
  Restarting state -> if state.count == state.total then getRandomCatGif else Cmd.none
  _                -> Cmd.none

gotGif : Model -> (HttpResult Url) -> Model
gotGif model res = case model.state of
  Restarting state ->
    let newModel = case res of
                     Ok  url -> { model | state = Done state (Maybe.Just url) }
                     Err err -> appendLog { model | state = Done state Maybe.Nothing } << printError <| err
    in appendLog newModel "Reached final state."
  _                -> appendLog model "Model in unexpected state, ignoring..."

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
     , printLog model
     ]
     ( case model.state of
         Init                 -> el [Font.size 30] ( text "Loading the app config..." )
         Ready cfg            -> viewReady model cfg
         AwaitConfirm cfg txt -> viewConfirm model cfg txt
         Restarting st        -> viewRestarted model st Maybe.Nothing
         Done st url          -> viewRestarted model st url
     )

viewReady : Model -> Config -> Element Msg
viewReady model cfg =
  column [ Element.width fill
         , Element.height fill
         ]
         [ el [ Element.centerX ]
              ( text "Secure the servers in your project" )
         , el [ Element.centerX
              , Element.centerY
              ]
              ( button "Restart the servers" button_url (RestartMsg cfg) )
         ]

viewConfirm : Model -> Config -> String -> Element Msg
viewConfirm model cfg txt =
  let confirmText = "YES"
  in column [ Element.width fill
           , Element.height fill
           ]
           [ el [ Element.centerX ]
                ( text "Confirmation required" )
           , el [ Element.centerX
                , Element.centerY
                ]
                ( Input.text [ Font.color (rgb255 0 0 0)
                             , Element.focused []
                             ]
                             { onChange = \s -> if s == confirmText
                                                then ConfirmMsg cfg
                                                else UpdateConfirmText s
                             , text = txt
                             , placeholder = Maybe.Nothing
                             , label = Input.labelAbove [] (text <| "Please type " ++ confirmText ++ " below to confirm")
                             }
                )
           ]

viewRestarted : Model -> RestartedState -> Maybe Url -> Element Msg
viewRestarted model state maybe_url =
  column [ Element.width fill
         , Element.height fill
         ]
         [ column [ Element.centerX
                  , Element.centerY
                  , Element.spacing 10
                  ]
                  [ text ("Restarting " ++ (String.fromInt state.total) ++ " servers.")
                  , text ("Progress: " ++ (String.fromInt state.count) ++ "/" ++ (String.fromInt state.total))
                  , Maybe.withDefault Element.none << Maybe.map renderImg <| maybe_url
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
  Element.behindContent (el [ Element.alignBottom
                            , Element.height fill
                            , Element.width fill
                            , Element.scrollbarY
                            , Font.size 10
                            ]
                            ( column []
                                     [ text "Debug log:\n"
                                     , text << String.join "\n" <| model.log
                                     ]
                            )
                        )

button_url : String
button_url = "red-button.png"

backgroundColor : Element.Color
backgroundColor = rgb255 100 50 50

fontColor : Element.Color
fontColor = rgb255 200 220 220

button : String -> String -> Msg -> Element Msg
button label src onPress =
  image [ Events.onClick onPress
        , Font.color       (rgb255 255 255 255)
        , Element.focused [ Background.color (rgb255 200 100 100) ]
        ]
        { src = src, description = label }

getConfig : Cmd Msg
getConfig = Http.get { url = UB.relative ["static", "api", "config"] []
                     , expect = Http.expectJson ConfigMsg configDecoder
                     }

configDecoder : Decoder Config
configDecoder = J.map Config << J.field "servers" << J.list << J.map Host <| J.string

restartServer : Host -> Cmd Msg
restartServer host = Http.get { url = UB.relative ["static", "api", "restart"] [UB.string "host" (fromHost host)]
                                --url = UB.crossOrigin ("https://" ++ name) ["api", "restart"] []
                              -- , body = Http.emptyBody
                              , expect = Http.expectJson (RestartDoneMsg host) (restartedDecoder host)
                              }

restartedDecoder : Host -> Decoder String
restartedDecoder (Host name) = J.map (\status -> name ++ ": " ++ status) << J.field "status" <| J.string

getRandomCatGif : Cmd Msg
getRandomCatGif = Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
                           , expect = Http.expectJson FoundGifMsg gifDecoder
                           }

gifDecoder : Decoder Url
gifDecoder = J.field "data" << J.field "image_url" << J.map Url <| J.string


