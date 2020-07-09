module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Content exposing (explanationText, lockingFailedText, progressText)
import Element as Element
    exposing
        ( Color
        , Element
        , alignBottom
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , maximum
        , newTabLink
        , padding
        , paragraph
        , rgb255
        , row
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as J exposing (Decoder)
import Markdown as MD
import Process as P
import SHA256 as SHA
import Task as T
import Time
import Url.Builder as UB


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> { title = "Panic Button", body = [ view model ] }
        }


confirmationInputId : String
confirmationInputId =
    "confirmationInputTextId"


confirmationTriggerText : String
confirmationTriggerText =
    "YES"


confirmationTriggered : AwaitConfirmState -> Bool
confirmationTriggered state =
    state.confirmationTxt == confirmationTriggerText


hostStatusOK : String -> Bool
hostStatusOK status =
    status == "OK"


maxWidth : Int
maxWidth =
    600


black : Color
black =
    rgb255 0 0 0


grey : Color
grey =
    rgb255 234 237 243


darkGrey : Color
darkGrey =
    rgb255 224 227 233


white : Color
white =
    rgb255 255 255 255


red : Color
red =
    rgb255 238 0 0


backgroundColor : Color
backgroundColor =
    white


buttonBackgroundColor : Color
buttonBackgroundColor =
    white


fontColor : Color
fontColor =
    black


msfLogoPath : Path
msfLogoPath =
    [ "static", "assets", "azg-logo.svg" ]


pluralise : String -> Int -> String
pluralise str num =
    if num == 1 then
        str

    else
        str ++ "s"


type Url
    = Url String


type Host
    = Host String


type alias Path =
    List String


fromUrl : Url -> String
fromUrl (Url url) =
    url


fromHost : Host -> String
fromHost (Host host) =
    host


type alias Log =
    List String


type alias Config =
    { hosts : List Host
    , retryDelaySec : Int
    , lockRetryMaxCount : Int
    , verifyRetryMaxCount : Int
    }


type alias ProgressState =
    Int


type alias AwaitConfirmState =
    { confirmationTxt : String
    , log : Log
    , config : Config
    , mock : Bool
    }


type alias LockingState =
    { log : Log
    , config : Config
    , mock : Bool
    , lockingProgress : ProgressState
    , verifyingProgress : ProgressState
    , failed : List ( Host, String )
    }


type Model
    = Init Log
    | AwaitConfirm AwaitConfirmState
    | Locking LockingState


type alias AppConfigResponse =
    { hosts : List Host
    , retryDelaySec : Int
    , lockRetryMaxCount : Int
    , verifyRetryMaxCount : Int
    }


type alias RequestContext =
    { host : Host
    , retryCount : Int
    , getMaxRetryCount : LockingState -> Int
    }


type alias HttpResult res =
    Result Http.Error res


type Msg
    = AppConfigMsg (HttpResult AppConfigResponse)
    | ConfirmMsg
    | LockDoneMsg RequestContext (HttpResult String)
    | VerifyDoneMsg RequestContext (HttpResult String)
    | RetryMsg RequestContext (Cmd Msg)
    | TagRequestMsg (Time.Posix -> Cmd Msg) Time.Posix
    | UpdateConfirmTextMsg String
    | UpdateMockCheckboxMsg Bool
    | FocusedMsg String


printError : Http.Error -> String
printError error =
    case error of
        Http.BadUrl s ->
            "BadUrl: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus i ->
            "BadStatus: " ++ String.fromInt i

        Http.BadBody s ->
            "BadBody: " ++ s


appendLog : Model -> String -> Model
appendLog model msg =
    case model of
        Init log ->
            Init <| log ++ [ msg ]

        AwaitConfirm state ->
            AwaitConfirm { state | log = state.log ++ [ msg ] }

        Locking state ->
            Locking <| appendLogLocking state msg


appendLogLocking : LockingState -> String -> LockingState
appendLogLocking state msg =
    { state | log = state.log ++ [ msg ] }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init [], getHostConfig )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( AppConfigMsg config, Init log ) ->
            ( gotAppConfig log config, tryFocus confirmationInputId )

        ( ConfirmMsg, AwaitConfirm state ) ->
            gotConfirm state

        ( LockDoneMsg host result, Locking state ) ->
            gotLockDone state host result

        ( VerifyDoneMsg host result, Locking state ) ->
            gotVerifyDone state host result

        ( RetryMsg ctxt retryCmd, Locking state ) ->
            gotRetry state ctxt retryCmd

        ( TagRequestMsg mkRequest time, mdl ) ->
            ( mdl, mkRequest time )

        ( UpdateConfirmTextMsg txt, AwaitConfirm state ) ->
            ( AwaitConfirm { state | confirmationTxt = txt }, Cmd.none )

        ( UpdateMockCheckboxMsg mock, AwaitConfirm state ) ->
            ( AwaitConfirm { state | mock = mock }, Cmd.none )

        ( FocusedMsg id, mdl ) ->
            ( appendLog mdl <| "Focused element with id=" ++ id, Cmd.none )

        ( _, mdl ) ->
            ( appendLog mdl <| "Ignoring unexpected message", Cmd.none )


gotAppConfig : Log -> HttpResult AppConfigResponse -> Model
gotAppConfig log res =
    case res of
        Ok { hosts, retryDelaySec, lockRetryMaxCount, verifyRetryMaxCount } ->
            let
                newLog =
                    log ++ ("Servers to lock:" :: List.map fromHost hosts)

                config =
                    { hosts = hosts
                    , retryDelaySec = retryDelaySec
                    , lockRetryMaxCount = lockRetryMaxCount
                    , verifyRetryMaxCount = verifyRetryMaxCount
                    }
            in
            AwaitConfirm
                { confirmationTxt = ""
                , log = newLog
                , config = config
                , mock = True
                }

        Err err ->
            Init <| log ++ [ printError err ]


gotConfirm : AwaitConfirmState -> ( Model, Cmd Msg )
gotConfirm state =
    let
        newState =
            { log = state.log ++ [ "Locking the servers..." ]
            , config = state.config
            , mock = state.mock
            , lockingProgress = 0
            , verifyingProgress = 0
            , failed = []
            }
    in
    ( Locking newState, doLock newState )


initRequestContext : (Config -> Int) -> Host -> RequestContext
initRequestContext getMaxRetryCount host =
    { host = host, retryCount = 0, getMaxRetryCount = getMaxRetryCount << .config }


doLock : LockingState -> Cmd Msg
doLock state =
    Cmd.batch
        << List.map
            (flip lockServer state.mock << initRequestContext .lockRetryMaxCount)
    <|
        state.config.hosts


gotLockDone : LockingState -> RequestContext -> HttpResult String -> ( Model, Cmd Msg )
gotLockDone state ctxt res =
    let
        newCmd =
            verifyServer (initRequestContext .verifyRetryMaxCount ctxt.host) state.mock

        mkRetryCmd c =
            lockServer c state.mock

        increaseLockingCount st =
            { st | lockingProgress = st.lockingProgress + 1 }
    in
    gotLockingProgress state ctxt res mkRetryCmd "Lock" increaseLockingCount newCmd


gotVerifyDone : LockingState -> RequestContext -> HttpResult String -> ( Model, Cmd Msg )
gotVerifyDone state ctxt res =
    let
        newCmd =
            Cmd.none

        mkRetryCmd c =
            verifyServer c state.mock

        increaseVerifyingCount st =
            { st | verifyingProgress = st.verifyingProgress + 1 }
    in
    gotLockingProgress state ctxt res mkRetryCmd "Verify" increaseVerifyingCount newCmd


gotLockingProgress :
    LockingState
    -> RequestContext
    -> HttpResult String
    -> (RequestContext -> Cmd Msg)
    -> String
    -> (LockingState -> LockingState)
    -> Cmd Msg
    -> ( Model, Cmd Msg )
gotLockingProgress state ctxt result mkRetryCmd logHeader incrProgress newCmd =
    let
        doLog =
            formatProgressMsg ctxt.host logHeader

        doRetry m =
            retry m ctxt mkRetryCmd
    in
    case result of
        Ok hostStatus ->
            if hostStatusOK hostStatus then
                ( Locking << incrProgress << appendLogLocking state <| doLog "success", newCmd )

            else
                doRetry << appendLogLocking state <| doLog "unsuccessful"

        Err err ->
            doRetry <| progressError state err doLog


progressError : LockingState -> Http.Error -> (String -> String) -> LockingState
progressError state err doLog =
    appendLogLocking state << doLog << printError <| err


formatProgressMsg : Host -> String -> String -> String
formatProgressMsg host logHeader msg =
    logHeader ++ ": " ++ fromHost host ++ ": " ++ msg


gotRetry : LockingState -> RequestContext -> Cmd Msg -> ( Model, Cmd Msg )
gotRetry state { host, retryCount } retryCmd =
    ( Locking << appendLogLocking state <| "Retrying: " ++ fromHost host ++ ", count: " ++ String.fromInt retryCount, retryCmd )


retry : LockingState -> RequestContext -> (RequestContext -> Cmd Msg) -> ( Model, Cmd Msg )
retry state ctxt mkRetryCmd =
    if ctxt.retryCount < ctxt.getMaxRetryCount state then
        let
            newCtxt =
                { ctxt | retryCount = ctxt.retryCount + 1 }

            retryMsg =
                RetryMsg newCtxt <| mkRetryCmd newCtxt
        in
        ( Locking state
        , T.perform (\_ -> retryMsg) << P.sleep << toFloat <| state.config.retryDelaySec * 1000
        )

    else
        let
            log =
                formatProgressMsg ctxt.host "Retry" "max retry count reached, giving up"

            addFailed st =
                { st | failed = ( ctxt.host, lockingFailedText ) :: st.failed }
        in
        ( Locking << addFailed <| appendLogLocking state log, Cmd.none )


tryFocus : String -> Cmd Msg
tryFocus id =
    T.attempt (\_ -> FocusedMsg id) <| Dom.focus id


getHostConfig : Cmd Msg
getHostConfig =
    Http.get
        { url = UB.relative [ "api", "config" ] []
        , expect = Http.expectJson AppConfigMsg appConfigDecoder
        }


appConfigDecoder : Decoder AppConfigResponse
appConfigDecoder =
    J.map4 AppConfigResponse
        (J.field "hosts" << J.list << J.map Host <| J.string)
        (J.field "retryDelaySec" J.int)
        (J.field "lockRetryMaxCount" J.int)
        (J.field "verifyRetryMaxCount" J.int)


{-| Tag a request with a key based off the current time
See the paramFromTime function
-}
tagRequest : (Time.Posix -> Cmd Msg) -> Cmd Msg
tagRequest mkRequest =
    T.perform (TagRequestMsg mkRequest) Time.now


lockServer : RequestContext -> Bool -> Cmd Msg
lockServer ctxt mock =
    tagRequest <| doLockServer ctxt mock


doLockServer : RequestContext -> Bool -> Time.Posix -> Cmd Msg
doLockServer ctxt mock time =
    doPost ctxt.host mock time [ "lock" ] (LockDoneMsg ctxt)


verifyServer : RequestContext -> Bool -> Cmd Msg
verifyServer ctxt mock =
    tagRequest <| doVerifyServer ctxt mock


doVerifyServer : RequestContext -> Bool -> Time.Posix -> Cmd Msg
doVerifyServer ctxt mock time =
    doGet ctxt.host mock time [ "verify" ] (VerifyDoneMsg ctxt)


doGet : Host -> Bool -> Time.Posix -> Path -> (HttpResult String -> Msg) -> Cmd Msg
doGet host mock time path mkMsg =
    let
        url =
            urlBuilder host
                ("api" :: path)
                [ paramFromBool "mock" mock
                , paramFromTime "key" time
                ]
    in
    Http.get
        { url = url
        , expect = Http.expectJson mkMsg statusDecoder
        }


doPost : Host -> Bool -> Time.Posix -> Path -> (HttpResult String -> Msg) -> Cmd Msg
doPost host mock time path mkMsg =
    let
        url =
            urlBuilder host
                ("api" :: path)
                [ paramFromBool "mock" mock
                , paramFromTime "key" time
                ]
    in
    Http.post
        { url = url
        , body = Http.emptyBody
        , expect = Http.expectJson mkMsg statusDecoder
        }


{-| When given the special value "<localhost>" for the hostname, we make a relative request.
-}
urlBuilder : Host -> Path -> List UB.QueryParameter -> String
urlBuilder host =
    case host of
        Host "<localhost>" ->
            UB.relative

        Host hostname ->
            UB.crossOrigin ("http://" ++ hostname)


paramFromBool : String -> Bool -> UB.QueryParameter
paramFromBool name value =
    UB.string name <|
        if value then
            "true"

        else
            "false"


{-| Tag a request with a key based off the current time which can be validated in the backend.
We divide by 2000 to have windows with a width of 2 seconds in which a request can be send,
and we can accept requests with a key corresponding to the previous window to avoid
edge cases when a request is sent right before a new window starts.
This validation of the key will serve as a basic protection against curious employees.
-}
paramFromTime : String -> Time.Posix -> UB.QueryParameter
paramFromTime name time =
    UB.string name
        << SHA.toHex
        << SHA.fromString
        << String.fromInt
    <|
        Time.posixToMillis time
            // 2000


statusDecoder : Decoder String
statusDecoder =
    J.field "status" J.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        focusStyle =
            { borderColor = Just black
            , backgroundColor = Nothing
            , shadow = Nothing
            }

        content =
            viewElement model
    in
    Element.layoutWith { options = [ Element.focusStyle focusStyle ] }
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
    let
        titleElement =
            wrappedRow
                [ centerX
                , padding 15
                , Font.size 50
                , Font.center
                ]
                -- The space behind the first part causes it to be slightly
                -- off-center on narrow screens when the title is wrapped.
                -- We could apply padding instead of a space to separate the two sections.
                [ el [ width fill ] <| text "MSF server "
                , el [ width fill, Font.color red ] <| text "panic button"
                ]

        mainElement =
            case model of
                Init _ ->
                    el [ centerX ] <| text "Loading the app config..."

                AwaitConfirm state ->
                    viewConfirm state

                Locking state ->
                    viewProgress state

        msfImage =
            image
                [ height << Element.minimum 50 <| fill
                , width fill
                , alignBottom
                ]
                { src = UB.relative msfLogoPath []
                , description = "MSF logo"
                }

        msfLogo =
            el
                [ alignBottom
                , width fill
                , padding 10
                , printLog model
                ]
            <|
                newTabLink [ centerX ]
                    { url = "https://www.msf-azg.be"
                    , label = msfImage
                    }
    in
    column
        [ width fill
        , height fill
        , spacing 15
        ]
        [ titleElement
        , el
            [ centerX
            , centerY
            , width <| maximum maxWidth fill
            ]
            mainElement
        , msfLogo
        ]


markdownPane : String -> Element Msg
markdownPane markdownText =
    paragraph
        [ centerX
        , padding 10
        , Font.justify
        ]
        [ Element.html << MD.toHtml [] <| markdownText ]


viewConfirm : AwaitConfirmState -> Element Msg
viewConfirm state =
    let
        ifConfirmed yes no =
            if confirmationTriggered state then
                yes

            else
                no

        goAction =
            ifConfirmed (Just ConfirmMsg) Nothing

        textLabel =
            paragraph []
                [ text "Please type "
                , el [ Font.bold, Font.color red ] <| text confirmationTriggerText
                , text " below to confirm"
                ]

        mockLabel =
            paragraph [] [ text "Test mode" ]

        mockCheckbox =
            Input.checkbox [ spacing 15 ]
                { onChange = UpdateMockCheckboxMsg
                , icon = Input.defaultCheckbox
                , checked = state.mock
                , label = Input.labelRight [ width fill ] mockLabel
                }

        textInput =
            Input.text
                [ Element.htmlAttribute << HA.id <| confirmationInputId
                , spacing 15
                , onEnter goAction
                ]
                { onChange = UpdateConfirmTextMsg
                , text = state.confirmationTxt
                , placeholder = Just << Input.placeholder [] << paragraph [] <| [ text confirmationTriggerText ]
                , label = Input.labelAbove [] textLabel
                }

        goButton =
            Input.button
                [ Border.width 1
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
    in
    column []
        [ column
            [ Border.width 1
            , Border.solid
            , Border.rounded 3
            , Border.color black
            , centerX
            , padding 20
            , spacing 15
            ]
            [ mockCheckbox
            , textInput
            , goButton
            ]
        , markdownPane explanationText
        ]


onEnter : Maybe msg -> Element.Attribute msg
onEnter =
    Element.htmlAttribute
        << HE.on "keyup"
        << Maybe.withDefault (J.fail "No message")
        << Maybe.map
            (\msg ->
                J.andThen
                    (\key ->
                        if key == "Enter" then
                            J.succeed msg

                        else
                            J.fail "Not the enter key"
                    )
                <|
                    J.field "key" J.string
            )


viewProgress : LockingState -> Element Msg
viewProgress state =
    let
        mockParagraph =
            if state.mock then
                paragraph []
                    [ text "Beware: you selected "
                    , el [ Font.bold, Font.color red ] <| text "test mode"
                    , text ", no servers will actually be disabled!"
                    ]

            else
                Element.none

        failedParagraph ( Host host, msg ) =
            paragraph []
                [ text <| host ++ ": "
                , el [ Font.color red ] <| text msg
                ]

        headerParagraph s =
            let
                numberOfServers =
                    List.length s.config.hosts
            in
            paragraph []
                [ text <|
                    "Locking "
                        ++ String.fromInt numberOfServers
                        ++ " "
                        ++ pluralise "server" numberOfServers
                        ++ "."
                ]
    in
    column []
        [ column
            [ Border.width 1
            , Border.solid
            , Border.rounded 3
            , Border.color black
            , centerX
            , padding 20
            , spacing 15
            ]
            [ mockParagraph
            , column [ Font.alignLeft ]
                [ headerParagraph state
                , paragraph [] [ text <| "Locked: " ++ printProgress state .lockingProgress ]
                , paragraph [] [ text <| "Verified: " ++ printProgress state .verifyingProgress ]
                ]
            , column [ Font.size 15 ] <| List.map failedParagraph state.failed
            ]
        , markdownPane progressText
        ]


printProgress : LockingState -> (LockingState -> Int) -> String
printProgress state getCount =
    (String.fromInt << getCount <| state) ++ "/" ++ (String.fromInt << List.length <| state.config.hosts)


printLog : Model -> Element.Attribute Msg
printLog model =
    Element.behindContent
        << Lazy.lazy doPrintLog
    <|
        case model of
            Init log ->
                log

            AwaitConfirm state ->
                state.log

            Locking state ->
                state.log


doPrintLog : List String -> Element Msg
doPrintLog msgs =
    let
        formatLine msg =
            paragraph [] [ text msg ]

        logLines =
            List.map formatLine <| "Debug log:" :: msgs
    in
    el
        [ height fill
        , width fill
        , padding 5
        , Font.size 3
        ]
    <|
        column [ alignBottom ] logLines
