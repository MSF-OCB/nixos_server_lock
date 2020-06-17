module Main exposing (main)

import Browser
import Browser.Dom as Dom
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
import Process as P
import SHA256 as SHA
import Task as T
import Time
import Url.Builder as UB



{-
   TODO items

   2. Message to inform the user that everything is done
   3. Detailed error messages, this may require a dict like mentioned in item 1

   Done

   1. Implement a maximum for the number of retries.
        -> this might require maintaining a dict in the model containing info per host
-}


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


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


confirmationTriggered : String -> Bool
confirmationTriggered input =
    input == confirmationTriggerText


hostStatusOK : String -> Bool
hostStatusOK status =
    status == "OK"


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


type ModelState
    = Init
    | AwaitConfirm String
    | Locking


type alias Model =
    { state : ModelState
    , config : Config
    , progress : Progress
    , log : List String
    }


type alias Config =
    { hosts : List Host
    , retryDelaySec : Int
    , lockRetryMaxCount : Int
    , verifyRetryMaxCount : Int
    , mock : Bool
    }


type alias AppConfigResponse =
    { hosts : List Host
    , retryDelaySec : Int
    , lockRetryMaxCount : Int
    , verifyRetryMaxCount : Int
    }


type alias ProgressState =
    Int


type alias Progress =
    { lockingProgress : ProgressState
    , verifyingProgress : ProgressState
    , failed : List ( Host, String )
    }


type alias RequestContext =
    { host : Host
    , retryCount : Int
    , getMaxRetryCount : Model -> Int
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
    { model | log = model.log ++ [ msg ] }


appendLogs : Model -> List String -> Model
appendLogs =
    List.foldl <| flip appendLog


updateConfig : (Config -> Config) -> Model -> Model
updateConfig doUpdate model =
    { model | config = doUpdate model.config }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getHostConfig )


initModel : Model
initModel =
    { state = Init
    , config =
        { hosts = []
        , retryDelaySec = -1
        , lockRetryMaxCount = -1
        , verifyRetryMaxCount = -1
        , mock = True
        }
    , progress =
        { lockingProgress = 0
        , verifyingProgress = 0
        , failed = []
        }
    , log = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppConfigMsg config ->
            ( gotAppConfig model config, tryFocus confirmationInputId )

        ConfirmMsg ->
            gotConfirm model

        LockDoneMsg host result ->
            gotLockDone model host result

        VerifyDoneMsg host result ->
            gotVerifyDone model host result

        RetryMsg ctxt retryCmd ->
            gotRetry model ctxt retryCmd

        TagRequestMsg mkRequest time ->
            ( model, mkRequest time )

        UpdateConfirmTextMsg txt ->
            ( { model | state = AwaitConfirm txt }, Cmd.none )

        UpdateMockCheckboxMsg mock ->
            ( updateConfig (\oldConfig -> { oldConfig | mock = mock }) model, Cmd.none )

        FocusedMsg id ->
            ( appendLog model <| "Focused element with id=" ++ id, Cmd.none )


gotAppConfig : Model -> HttpResult AppConfigResponse -> Model
gotAppConfig model res =
    case res of
        Ok { hosts, retryDelaySec, lockRetryMaxCount, verifyRetryMaxCount } ->
            let
                addLogs =
                    flip appendLogs <| "Servers to lock:" :: List.map fromHost hosts

                setFields =
                    updateConfig <|
                        \oldConfig ->
                            { oldConfig
                                | hosts = hosts
                                , retryDelaySec = retryDelaySec
                                , lockRetryMaxCount = lockRetryMaxCount
                                , verifyRetryMaxCount = verifyRetryMaxCount
                            }
            in
            addLogs << setFields <| { model | state = AwaitConfirm "" }

        Err err ->
            appendLog model << printError <| err


gotConfirm : Model -> ( Model, Cmd Msg )
gotConfirm model =
    let
        setLocking m =
            { m
                | state =
                    Locking
                , progress =
                    { lockingProgress = 0
                    , verifyingProgress = 0
                    , failed = []
                    }
            }

        addLog =
            flip appendLog "Locking the servers..."
    in
    ( addLog << setLocking <| model, doLock model.config )


initRequestContext : (Config -> Int) -> Host -> RequestContext
initRequestContext getMaxRetryCount host =
    { host = host, retryCount = 0, getMaxRetryCount = getMaxRetryCount << .config }


doLock : Config -> Cmd Msg
doLock cfg =
    Cmd.batch << List.map (flip lockServer cfg.mock << initRequestContext .lockRetryMaxCount) <| cfg.hosts


gotLockDone : Model -> RequestContext -> HttpResult String -> ( Model, Cmd Msg )
gotLockDone model ctxt res =
    let
        newCmd =
            verifyServer (initRequestContext .verifyRetryMaxCount ctxt.host) model.config.mock

        mkRetryCmd c =
            lockServer c model.config.mock

        increaseLockingCount progress =
            { progress | lockingProgress = progress.lockingProgress + 1 }
    in
    gotLockingProgress model ctxt res mkRetryCmd "Lock" increaseLockingCount newCmd


gotVerifyDone : Model -> RequestContext -> HttpResult String -> ( Model, Cmd Msg )
gotVerifyDone model ctxt res =
    let
        newCmd =
            Cmd.none

        mkRetryCmd c =
            verifyServer c model.config.mock

        increaseVerifyingCount progress =
            { progress | verifyingProgress = progress.verifyingProgress + 1 }
    in
    gotLockingProgress model ctxt res mkRetryCmd "Verify" increaseVerifyingCount newCmd


gotLockingProgress :
    Model
    -> RequestContext
    -> HttpResult String
    -> (RequestContext -> Cmd Msg)
    -> String
    -> (Progress -> Progress)
    -> Cmd Msg
    -> ( Model, Cmd Msg )
gotLockingProgress model ctxt result mkRetryCmd logHeader incrProgress newCmd =
    let
        doLog =
            formatProgressMsg ctxt.host logHeader

        doRetry m =
            retry m ctxt mkRetryCmd
    in
    case result of
        Ok hostStatus ->
            if hostStatusOK hostStatus then
                newProgressModel (appendLog model <| doLog "success") incrProgress newCmd

            else
                doRetry (appendLog model <| doLog "unsuccessful")

        Err err ->
            doRetry <| progressError model err doLog


newProgressModel : Model -> (Progress -> Progress) -> Cmd Msg -> ( Model, Cmd Msg )
newProgressModel model incr newCmd =
    ( { model | progress = incr model.progress }, newCmd )


progressError : Model -> Http.Error -> (String -> String) -> Model
progressError model err doLog =
    appendLog model << doLog << printError <| err


formatProgressMsg : Host -> String -> String -> String
formatProgressMsg host logHeader msg =
    logHeader ++ ": " ++ fromHost host ++ ": " ++ msg


gotRetry : Model -> RequestContext -> Cmd Msg -> ( Model, Cmd Msg )
gotRetry model { host, retryCount } retryCmd =
    ( appendLog model <| "Retrying: " ++ fromHost host ++ ", count: " ++ String.fromInt retryCount, retryCmd )


retry : Model -> RequestContext -> (RequestContext -> Cmd Msg) -> ( Model, Cmd Msg )
retry model ctxt mkRetryCmd =
    let
        { host, retryCount } =
            ctxt

        newCtxt =
            { ctxt | retryCount = retryCount + 1 }

        retryCmd =
            mkRetryCmd newCtxt
    in
    if retryCount < ctxt.getMaxRetryCount model then
        ( model, T.perform (\_ -> RetryMsg newCtxt retryCmd) << P.sleep << toFloat <| model.config.retryDelaySec * 1000 )

    else
        let
            doLog =
                formatProgressMsg host "Retry" "max retry count reached, giving up"

            incrProgress progress =
                { progress | failed = ( host, "This server could not be locked, please contact your IT staff." ) :: progress.failed }
        in
        newProgressModel (appendLog model doLog) incrProgress Cmd.none


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
        title =
            wrappedRow
                [ width fill
                , Font.size 50
                , Font.center
                ]
                [ el [ width fill ] <| text "MSF server "
                , el [ width fill, Font.color red ] <| text "panic button"
                ]

        titleElement =
            el
                [ centerX
                , centerY
                ]
            <|
                title

        mainElement =
            el
                [ height fill
                , width fill
                ]
            <|
                case model.state of
                    Init ->
                        el [] <| text "Loading the app config..."

                    AwaitConfirm txt ->
                        viewConfirm model txt

                    Locking ->
                        viewProgress model

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
        , printLog model
        ]
        [ titleElement
        , mainElement
        , msfLogo
        ]


viewConfirm : Model -> String -> Element Msg
viewConfirm model txt =
    let
        ifConfirmed yes no =
            if confirmationTriggered txt then
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

        mockDescription =
            [ "Do a test run without actually locking the servers.", "Only uncheck this in a real emergency." ]

        mockLabel =
            "Test mode"

        mockCheckbox =
            Input.checkbox [ spacing 15 ]
                { onChange = UpdateMockCheckboxMsg
                , icon = Input.defaultCheckbox
                , checked = model.config.mock
                , label = Input.labelRight [ width fill ] <| paragraph [] [ text mockLabel ]
                }

        textInput =
            Input.text
                [ Element.htmlAttribute << HA.id <| confirmationInputId
                , spacing 15
                , onEnter goAction
                ]
                { onChange = UpdateConfirmTextMsg
                , text = txt
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
    column
        [ width fill
        , height fill
        ]
        [ column
            [ centerX
            , centerY
            , spacing 15
            ]
            [ paragraph [] [ text "<warning about the consequences>" ]
            , column
                [ Border.width 1
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


viewProgress : Model -> Element Msg
viewProgress model =
    let
        mockParagraph =
            if model.config.mock then
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

        headerParagraph m =
            let
                numberOfServers =
                    List.length m.config.hosts
            in
            paragraph []
                [ text <|
                    "Locking "
                        ++ String.fromInt numberOfServers
                        ++ " server"
                        ++ (if numberOfServers == 1 then
                                "."

                            else
                                "s."
                           )
                ]
    in
    column
        [ width fill
        , height fill
        ]
        [ el
            [ centerX
            , centerY
            ]
          <|
            column
                [ Border.width 1
                , Border.solid
                , Border.rounded 3
                , Border.color black
                , centerX
                , width fill
                , padding 20
                , spacing 15
                ]
                [ mockParagraph
                , column [ Font.alignLeft ]
                    [ headerParagraph model
                    , paragraph [] [ text <| "Locked: " ++ printProgress model .lockingProgress ]
                    , paragraph [] [ text <| "Verified: " ++ printProgress model .verifyingProgress ]
                    ]
                , column [ Font.size 15 ] <| List.map failedParagraph model.progress.failed
                ]
        ]


printProgress : Model -> (Progress -> Int) -> String
printProgress model getCount =
    (String.fromInt << getCount << .progress <| model) ++ "/" ++ (String.fromInt << List.length <| model.config.hosts)


printLog : Model -> Element.Attribute Msg
printLog model =
    Element.behindContent << Lazy.lazy doPrintLog <| model.log


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
        , Font.size 3
        ]
    <|
        column [ alignBottom ] logLines
