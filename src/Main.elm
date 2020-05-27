module Main exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as J exposing (Decoder)

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

type ModelState = Init
                | Ready Config
                | Restarting RestartedState
                | Done RestartedState (Maybe String)

type alias Model = { state : ModelState
                   , log   : List String
                   }

type alias Config = { hosts : List String }

type alias RestartedState = { total : Int
                            , count : Int
                            }

type Msg = ConfigMsg (Result Http.Error Config)
         | RestartMsg Config
         | RestartDoneMsg (Result Http.Error String)
         | FoundGifMsg (Result Http.Error String)

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
  ConfigMsg result      -> (gotConfig model result, Cmd.none)
  RestartMsg cfg        -> (gotRestart model cfg, doRestart cfg)
  RestartDoneMsg result -> let newModel = gotRestartDone model result
                           in (newModel, restartDoneCmd newModel)
  FoundGifMsg result    -> (gotGif model result, Cmd.none)

gotConfig : Model -> (Result Http.Error Config) -> Model
gotConfig model res = case res of
  Ok  cfg -> appendLogs { model | state = Ready cfg }
                        ("Servers to disable:" :: cfg.hosts)
  Err err -> appendLog model (printError err)

gotRestart : Model -> Config -> Model
gotRestart model cfg = appendLog { model | state = Restarting { total = List.length cfg.hosts, count = 0 } }
                                 ("Restarting servers...")

doRestart : Config -> Cmd Msg
doRestart cfg = Cmd.batch << (List.map restartServer) <| cfg.hosts

gotRestartDone : Model -> (Result Http.Error String) -> Model
gotRestartDone model res = case res of
  Ok  host_status -> case model.state of
    Restarting state -> appendLog { model | state = Restarting { state | count = state.count + 1 } }
                                  host_status
    _                -> appendLog model "Model in unexpected state, ignoring..."
  Err err  -> appendLog model << printError <| err

restartDoneCmd : Model -> Cmd Msg
restartDoneCmd model = case model.state of
  Restarting state -> if state.count == state.total then getRandomCatGif else Cmd.none
  _                -> Cmd.none

gotGif : Model -> (Result Http.Error String) -> Model
gotGif model res = case model.state of
  Restarting state ->
    let newModel = case res of
                     Ok  url -> { model | state = Done state (Maybe.Just url) }
                     Err err -> appendLog { model | state = Done state Maybe.Nothing} << printError <| err
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
         Init          -> el [Font.size 30] ( text "Loading the app config..." )
         Ready cfg     -> viewReady model cfg
         Restarting st -> viewRestarted model st Maybe.Nothing
         Done st url   -> viewRestarted model st url
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

viewRestarted : Model -> RestartedState -> Maybe String -> Element Msg
viewRestarted model state maybe_url =
  column [ Element.width fill
         , Element.height fill
         ]
         [ column [ Element.centerX
                  , Element.centerY
                  ]
                  [ text ("Restarting " ++ (String.fromInt state.total) ++ " servers.")
                  , text ("Progress: " ++ (String.fromInt state.count) ++ "/" ++ (String.fromInt state.total))
                  , Maybe.withDefault Element.none << Maybe.map renderImg <| maybe_url
                  ]
         ]

renderImg : String -> Element Msg
renderImg url = image [] { src = url, description = "Funny cat, we're done!" }

printLog : Model -> Element.Attribute Msg
printLog model =
  Element.behindContent (el [ Element.alignBottom
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
fontColor = rgb255 200 250 250

button : String -> String -> Msg -> Element Msg
button label src onPress =
  image [ Events.onClick onPress
        , Font.color       (rgb255 255 255 255)
        , Element.focused [ Background.color (rgb255 200 100 100) ]
        ]
        { src = src, description = label }

getConfig : Cmd Msg
getConfig = Http.get { url = config_url
                     , expect = Http.expectJson ConfigMsg configDecoder
                     }

configDecoder : Decoder Config
configDecoder = J.map Config << J.field "servers" << J.list <| J.string

restartServer : String -> Cmd Msg
restartServer host = Http.get { url = restart_url ++ "?host=" ++ host --"https://" ++ host ++ "/api/restart"
                              -- , body = Http.emptyBody
                              , expect = Http.expectJson RestartDoneMsg (restartedDecoder host)
                              }

restartedDecoder : String -> Decoder String
restartedDecoder host = J.map (\status -> host ++ ": " ++ status) << J.field "status" <| J.string

getRandomCatGif : Cmd Msg
getRandomCatGif = Http.get { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
                           , expect = Http.expectJson FoundGifMsg gifDecoder
                           }

gifDecoder : Decoder String
gifDecoder = J.field "data" << J.field "image_url" <| J.string

config_url: String
config_url = "https://gist.githubusercontent.com/R-VdP/4947fc2c79918c2ae02398a326c3bc63/raw/fc4a2cc81f50e426275b7dff87c26d724439720d/config"

restart_url : String
restart_url = "https://gist.githubusercontent.com/R-VdP/9ef9c09fdc13cdde5c890ef46b0b5b79/raw/34e13504603db179d6531aa6724bee52932332c9/restart"

