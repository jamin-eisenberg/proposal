port module Main exposing (main)

import Browser
import Html exposing (button, div, img, input, text)
import Html.Attributes exposing (class, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import String
import Time


type alias Model =
    { step : Maybe Int, pendingPassword : String }


type Msg
    = ScanSucceeded NfcTag
    | ScanFailed String -- reason
    | ManualOverride
    | UpdatePendingPassword String


type NfcTag
    = Bench
    | SteeringWheel


port scanSucceeded : (Int -> msg) -> Sub msg


port scanFailed : (String -> msg) -> Sub msg


scanSucceededSub : Sub Msg
scanSucceededSub =
    scanSucceeded
        (\id ->
            let
                _ =
                    Debug.log <| "scan retrieved " ++ String.fromInt id
            in
            case stepNfcTag id of
                Just tag ->
                    ScanSucceeded tag

                _ ->
                    ScanFailed "Unknown tag scanned."
        )


stepNfcTag : Int -> Maybe NfcTag
stepNfcTag step =
    case step of
        0 ->
            Just Bench

        1 ->
            Just SteeringWheel

        _ ->
            Nothing


scanFailedSub : Sub Msg
scanFailedSub =
    scanFailed ScanFailed


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ scanSucceededSub, scanFailedSub ]
        }


init : a -> ( Model, Cmd msg )
init _ =
    ( { step = Just 0, pendingPassword = "" }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.step |> Maybe.andThen stepNfcTag of
        Nothing ->
            text "You win!"

        Just step ->
            div []
                [ input [ type_ "number", value model.pendingPassword, onInput UpdatePendingPassword ] []
                , button [ class "button button-primary", onClick ManualOverride ] [ text "Manual Override" ]
                , img [ src <| "images/" ++ Debug.toString step ++ ".png", style "width" "100%" ] []
                ]


incOrEnd : Maybe Int -> Maybe Int
incOrEnd step =
    let
        incremented =
            Maybe.map ((+) 1) step
    in
    incremented
        |> Maybe.andThen stepNfcTag
        |> Maybe.andThen (always incremented)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model.step of
        Nothing ->
            ( model, Cmd.none )

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
                        ( { model | step = incOrEnd model.step }, Cmd.none )

                    else
                        ( model, Cmd.none )

                ScanFailed reason ->
                    let
                        _ =
                            Debug.log "" reason
                    in
                    ( model, Cmd.none )

                ManualOverride ->
                    if model.pendingPassword == manualOverridePassword then
                        ( { model | step = incOrEnd model.step, pendingPassword = "" }, Cmd.none )

                    else
                        ( { model | pendingPassword = "" }, Cmd.none )

                UpdatePendingPassword newPassword ->
                    ( { model | pendingPassword = newPassword }, Cmd.none )


manualOverridePassword : String
manualOverridePassword =
    "6372"
