module Main exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as J exposing (Decoder)
import Task as T
import Process as P

import Element as Element exposing (Element, el, text, image, column, row, alignRight, fill, width, rgb255, spacing, centerY, padding)
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

type Model = Init
           | Ready Config
           | Restarted RestartedState
           | Failure FailureState

type alias Config = { hosts : List String }

type alias RestartedState = { total: Int, count : Int, log: List String }

type FailureState = HttpError Http.Error
                  | MsgError String

type Msg = GotConfig (Result Http.Error Config)
         | Restart Config
         | RestartDone (Result Http.Error String)

init : () -> (Model, Cmd Msg)
init _ = (Init, getConfig)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Restart cfg        -> (Restarted { total = List.length cfg.hosts, count = 0, log = [] },
                                   Cmd.batch (List.map restartServerDelayed cfg.hosts))
  GotConfig result   -> case result of
    Ok  cfg -> (Ready cfg, Cmd.none)
    Err err -> (Failure (HttpError err ), Cmd.none)
  RestartDone result -> case result of
    Ok  host -> case model of
      Restarted state -> (Restarted { state | count = state.count + 1, log = host :: state.log} , Cmd.none)
      _               -> (Failure (MsgError "Model in unexpected state"), Cmd.none)
    Err err  -> (Failure (HttpError err), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model = Element.layout [ Background.color backgroundColor
                            , Font.color fontColor
                            , Font.size 50
                            ] (viewElement model)

viewElement : Model -> Element Msg
viewElement model = case model of
  Init         -> el [] ( text "Loading the app config..." )
  Ready cfg    -> viewReady cfg
  Restarted st -> viewRestarted st
  Failure fs   -> el [] ( text (Debug.toString fs) )

viewReady : Config -> Element Msg
viewReady cfg =
  column [ Element.width fill
         , Element.height fill
         ]
         [ el [ Element.centerX ]
              ( text "Secure the servers in your project" )
         , el [ Element.centerX
              , Element.centerY
              ]
              ( button "Restart the servers" button_url (Restart cfg) )
         , el [ Element.alignBottom
              , Font.size 10
              ]
              ( column []
                       [ text "Debug log:\n"
                       , text ("Servers to disable:\n" ++ String.join "\n" cfg.hosts)
                       ]
              )
         ]

viewRestarted : RestartedState -> Element Msg
viewRestarted state = column [ Element.width fill
                             , Element.height fill
                             ]
                             [ column [ Element.centerX
                                      , Element.centerY
                                      ]
                                      [ text ("Restarting " ++ (String.fromInt state.total) ++ " servers.")
                                      , text ("Progress: " ++ (String.fromInt state.count) ++ "/" ++ (String.fromInt state.total))
                                      ]
                             , el [ Element.alignBottom
                                  , Font.size 10
                                  ]
                                  ( column []
                                           [ text "Debug log:\n"
                                           , text ("Servers restarted:\n" ++ String.join "\n" state.log)
                                           ]
                                  )
                             ]

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
                     , expect = Http.expectJson GotConfig configDecoder
                     }

configDecoder : Decoder Config
configDecoder = J.map Config (J.field "servers" (J.list J.string))

restartServerDelayed : String -> Cmd Msg
restartServerDelayed host = T.perform (\_ -> RestartDone (Ok host)) (P.sleep (3*1000))

restartServer : String -> Cmd Msg
restartServer host = Http.get { url = config_url --"https://" ++ host ++ "/api/restart"
                              -- , body = Http.emptyBody
                               , expect = Http.expectJson RestartDone restartedDecoder
                               }

restartedDecoder : Decoder String
restartedDecoder = J.succeed "Unimplemented"

config_url: String
config_url = "https://gist.githubusercontent.com/R-VdP/4947fc2c79918c2ae02398a326c3bc63/raw/39bf796ec9b62c8266fc3725ab254b32d57b5d10/config"


