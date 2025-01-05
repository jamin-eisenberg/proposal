port module Main exposing (main)

import Browser
import Html exposing (img, text)
import Html.Attributes exposing (src, style, width)
import String
import Time


type alias Model =
    { step : Int }


type Msg
    = ScanSucceeded NfcTag
    | ScanFailed String -- reason
    | ManualOverride


type NfcTag
    = Bench


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


init _ =
    ( { step = 0 }, Cmd.none )


view model =
    img [ src <| "images/" ++ String.fromInt model.step ++ ".png", style "width" "100%" ] []


update msg model =
    case msg of
        ScanSucceeded nfcTag ->
            let
                _ =
                    Debug.log "" nfcTag
            in
            if stepNfcTag model.step == Just nfcTag then
                ( { model | step = model.step + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        ScanFailed reason ->
            let
                _ =
                    Debug.log "" reason
            in
            ( model, Cmd.none )

        ManualOverride ->
            ( { model | step = model.step + 1 }, Cmd.none )
