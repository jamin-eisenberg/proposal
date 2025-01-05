port module Main exposing (main)

import Browser
import Html exposing (text)
import String
import Time


type alias Model =
    {}


type Msg
    = StartScan
    | ScanSucceeded NfcTag
    | ScanFailed String -- reason


type NfcTag
    = Bench


port scan : () -> Cmd msg


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
            case id of
                0 ->
                    ScanSucceeded Bench

                _ ->
                    ScanFailed "Unknown tag scanned."
        )


scanFailedSub : Sub Msg
scanFailedSub =
    scanFailed ScanFailed


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch [ scanSucceededSub, scanFailedSub, Time.every 500 (always StartScan) ]
        }


init _ =
    ( {}, Cmd.none )


view model =
    text "hi"


update msg model =
    case msg of
        StartScan ->
            ( {}, scan () )

        ScanSucceeded nfcTag ->
            let
                _ =
                    Debug.log "" nfcTag
            in
            ( {}, Cmd.none )

        ScanFailed reason ->
            let
                _ =
                    Debug.log "" reason
            in
            ( {}, Cmd.none )
