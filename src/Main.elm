module Main exposing (main)

import Browser
import Html exposing (text)



-- Model


type alias Model =
    { text : String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { text = "Hello World" }, Cmd.none )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Estimates"
    , body =
        [ text model.text ]
    }



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
