module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra exposing (getAt, setAt)



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
    ( { tasks =
            [ Task "test"
                3
                [ Task "subtask" 2 []
                , Task "another" 1 []
                ]
            ]
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | AddTask (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        AddTask path ->
            ( { model | tasks = addTask path model.tasks }, Cmd.none )


addTask : List Int -> Tasks -> Tasks
addTask path tasks =
    case Debug.log "path" path of
        index :: restOfPath ->
            let
                taskAtIndex =
                    getAt index tasks

                _ =
                    Debug.log "task" taskAtIndex

                _ =
                    Debug.log "index" index
            in
            if List.length restOfPath == 0 then
                tasks ++ [ Task "new task" 0 [] ]

            else
                case taskAtIndex of
                    Just (Task desc estimate subtasks) ->
                        setAt index (Task desc estimate (addTask restOfPath subtasks)) tasks

                    Nothing ->
                        tasks

        [] ->
            tasks



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Estimates"
    , body =
        [ Html.main_ [ class "app" ]
            [ Html.ul [ class "list" ] <|
                (model.tasks
                    |> List.indexedMap (taskView [] model.tasks)
                )
            ]
        ]
    }


taskView : List Int -> Tasks -> Int -> Task -> Html Msg
taskView path parentTasks index (Task description days tasks) =
    Html.li [ class "task" ]
        [ Html.span [] [ text (description ++ " " ++ String.fromInt days) ]
        , Html.ul [ class "subtasks" ] <| List.indexedMap (taskView (path ++ [ index ]) tasks) tasks
        , if List.length parentTasks == index + 1 then
            Html.button [ class "add", onClick (AddTask (path ++ [ index ])) ] [ text "add" ]

          else
            text ""
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
