module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, br, button, div, h1, img, pre, text, textarea)
import Html.Attributes exposing (cols, rows, src, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, at)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)



---- MODEL ----


type alias Model =
    { incomingJson : String
    , jsonError : Maybe String
    , timeoutPolicy : Maybe TimeoutPolicy
    }


type alias TimeoutPolicy =
    { entryPointName : String
    , timeUnit : String
    , value : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { incomingJson = ""
      , jsonError = Nothing
      , timeoutPolicy = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ParseJson
    | UpdateJson String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ParseJson ->
            ( decodeIncomingJson model, Cmd.none )

        UpdateJson val ->
            ( { model | incomingJson = val }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ value model.incomingJson
            , onInput UpdateJson
            , rows 10
            , cols 40
            ]
            []
        , button [ onClick ParseJson ] [ text "Parse" ]
        , br [] []
        , text (toString model)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


decodeIncomingJson : Model -> Model
decodeIncomingJson model =
    let
        json =
            model.incomingJson

        ( newTimeout, err ) =
            case String.length json > 0 of
                True ->
                    case Decode.decodeString decoderTimeoutPolicy json of
                        Ok timeout ->
                            ( Just timeout, Nothing )

                        Err e ->
                            ( model.timeoutPolicy
                            , Just (Decode.errorToString e)
                            )

                _ ->
                    ( model.timeoutPolicy, Just "No JSON" )
    in
    { model | timeoutPolicy = newTimeout, jsonError = err }



---- This parses
---- { "hours": 48 }
--decoderTimeoutPolicy : Decoder TimeoutPolicy
--decoderTimeoutPolicy =
--    Decode.succeed TimeoutPolicy
--        |> Pipeline.hardcoded "*"
--        |> Pipeline.hardcoded "hours"
--        |> Pipeline.requiredAt [ "hours" ] Decode.int


decoderTimeoutPolicy : Decoder TimeoutPolicy



-- This parses
-- { "hours": 48 }
-- or
-- { "minutes": 60 }


decoderTimeoutPolicy =
    Decode.oneOf
        [ Decode.succeed TimeoutPolicy
            |> Pipeline.hardcoded "*"
            |> Pipeline.hardcoded "hours"
            |> Pipeline.requiredAt [ "hours" ] Decode.int
        , Decode.succeed TimeoutPolicy
            |> Pipeline.hardcoded "*"
            |> Pipeline.hardcoded "minutes"
            |> Pipeline.requiredAt [ "minutes" ] Decode.int
        ]



--decoderTimeoutPolicy : Decoder TimeoutPolicy
--decoderTimeoutPolicy =
--    Decode.value
--        |> Decode.andThen
--            (\value ->
--                Decode.decodeValue
--                    (Decode.succeed TimeoutPolicy
--                        |> Pipeline.hardcoded "*"
--                        |> Pipeline.hardcoded "hours"
--                        |> Pipeline.requiredAt [ "hours" ] Decode.int
--                    )
--                    value
--            )
