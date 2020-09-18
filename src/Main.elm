module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class)



-- Model


type alias Model =
    { tasks : Tasks
    }


type alias Days =
    Int


type Task
    = Task String Days Tasks


type alias Tasks =
    List Task


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks = [ Task "test" 3 [ Task "subtask" 2 [] ] ] }, Cmd.none )



-- Update


type Msg
    = NoOp
    | AddTask


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddTask ->
            ( model, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Estimates"
    , body =
        [ Html.main_ [ class "app" ]
            [ Html.ul [ class "list" ] <|
                (model.tasks
                    |> List.map taskView
                )
            ]
        ]
    }


taskView : Task -> Html Msg
taskView (Task description days tasks) =
    Html.li [ class "task" ]
        [ Html.span [] [ text (description ++ " " ++ String.fromInt days) ]
        , Html.ul [ class "subtasks" ] <| List.map taskView tasks
        , Html.button [ class "add" ] [ text "add" ]
        ]



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
