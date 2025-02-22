port module Main exposing (main)

import Browser
import Html exposing (button, div, img, input, text)
import Html.Attributes exposing (class, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra
import String
import Task
import Toast


type alias Model =
    { step : Maybe OneIndexed
    , pendingPassword : String
    , tray : Toast.Tray String
    }


type alias SavedModel =
    { step : Maybe Int }


type OneIndexed
    = OneIndexed Int


type Msg
    = ScanSucceeded NfcTag
    | ScanFailed String -- reason
    | ManualOverride
    | UpdatePendingPassword String
    | ToastMsg Toast.Msg
    | ShowError String


type NfcTag
    = SteeringWheel
    | BrickBench
    | Tables
    | Classroom
    | Bench
    | Mints
    | OutWindow
    | Water
    | RingBox


port scanSucceeded : (Int -> msg) -> Sub msg


port scanFailed : (String -> msg) -> Sub msg


port saveModel : SavedModel -> Cmd msg


scanSucceededSub : Sub Msg
scanSucceededSub =
    scanSucceeded
        (\id ->
            let
                _ =
                    Debug.log <| "scan retrieved " ++ String.fromInt id
            in
            case stepNfcTag (OneIndexed id) of
                Just tag ->
                    ScanSucceeded tag

                _ ->
                    ScanFailed "Unknown tag scanned."
        )


stepNfcTag : OneIndexed -> Maybe NfcTag
stepNfcTag (OneIndexed step) =
    List.Extra.getAt (step - 1)
        [ SteeringWheel
        , BrickBench
        , Tables
        , Classroom
        , Bench
        , Mints
        , OutWindow
        , Water
        , RingBox
        ]


scanFailedSub : Sub Msg
scanFailedSub =
    scanFailed ScanFailed


main : Program SavedModel Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ scanSucceededSub, scanFailedSub ]
        }


init : SavedModel -> ( Model, Cmd msg )
init savedModel =
    ( { step = Maybe.map OneIndexed savedModel.step, pendingPassword = "", tray = Toast.tray }, Cmd.none )


viewToast : List (Html.Attribute Msg) -> Toast.Info String -> Html.Html Msg
viewToast attributes toast =
    Html.div (class "toast toast--spaced" :: attributes) [ Html.text toast.content ]


view : Model -> Html.Html Msg
view model =
    div []
        [ div [ class "toast-tray" ] [ Toast.render viewToast model.tray (Toast.config ToastMsg) ]
        , case model.step |> Maybe.andThen stepNfcTag of
            Nothing ->
                div []
                    [ Html.h1 [] [ text "❤️" ], text "(this part is done)" ]

            Just step ->
                div []
                    [ input [ type_ "number", value model.pendingPassword, onInput UpdatePendingPassword ] []
                    , button [ class "button button-primary", onClick ManualOverride ] [ text "Manual Override" ]
                    , img [ src <| "images/" ++ Debug.toString step ++ ".jpg", style "width" "100%" ] []
                    ]
        ]


toSavedModel : Model -> SavedModel
toSavedModel model =
    case model.step of
        Just (OneIndexed step) ->
            { step = Just step }

        Nothing ->
            { step = Nothing }


incOrEnd : Maybe OneIndexed -> Maybe OneIndexed
incOrEnd step =
    let
        incremented =
            Maybe.map (\(OneIndexed i) -> OneIndexed (i + 1)) step
    in
    incremented
        |> Maybe.andThen stepNfcTag
        |> Maybe.andThen (always incremented)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowError error ->
            let
                _ =
                    Debug.log "" <| "showing error: " ++ error

                ( tray, tmesg ) =
                    Toast.add model.tray (Toast.expireOnBlur 5000 error)
            in
            ( { model | tray = tray }, Cmd.map ToastMsg tmesg )

        ToastMsg tmsg ->
            let
                ( tray, newTmesg ) =
                    Toast.update tmsg model.tray
            in
            ( { model | tray = tray }, Cmd.map ToastMsg newTmesg )

        _ ->
            case model.step of
                Nothing ->
                    ( model, showError "You're done with this part of the proposal!" )

                Just step ->
                    case msg of
                        ScanSucceeded nfcTag ->
                            let
                                _ =
                                    Debug.log "" nfcTag
                            in
                            if
                                stepNfcTag step
                                    == Just nfcTag
                            then
                                let
                                    newModel =
                                        { model | step = incOrEnd model.step }
                                in
                                ( newModel, saveModel (toSavedModel newModel) )

                            else
                                ( model, showError "That's not the right thing to scan. You probably already scanned this one." )

                        ScanFailed reason ->
                            let
                                _ =
                                    Debug.log "" reason
                            in
                            ( model, showError <| "The scan failed: " ++ reason )

                        ManualOverride ->
                            if model.pendingPassword == manualOverridePassword then
                                let
                                    newModel =
                                        { model | step = incOrEnd model.step, pendingPassword = "" }
                                in
                                ( newModel, saveModel (toSavedModel newModel) )

                            else
                                ( { model | pendingPassword = "" }, showError "Wrong manual override password. It's really just meant for if the scanning isn't working." )

                        UpdatePendingPassword newPassword ->
                            ( { model | pendingPassword = newPassword }, Cmd.none )

                        _ ->
                            ( model, showError <| "Internal error! Unhandled message " ++ Debug.toString msg )


showError : String -> Cmd Msg
showError error =
    let
        _ =
            Debug.log <| "ERROR: " ++ error
    in
    Task.perform (\_ -> ShowError error) (Task.succeed ())


manualOverridePassword : String
manualOverridePassword =
    "6372"



-- tag 7 is in apt 58
