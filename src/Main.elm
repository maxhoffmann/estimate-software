module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra exposing (getAt, setAt)



-- Model


type alias Model =
    { tasks : List Item
    }


type Item
    = Task String (List Item)
    | Subtask String Int


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks =
            [ Task "test"
                [ Subtask "subtask" 2
                , Subtask "another" 1
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


addTask : List Int -> List Item -> List Item
addTask path tasks =
    case path of
        index :: restOfPath ->
            let
                taskAtIndex =
                    getAt index tasks
            in
            if List.length restOfPath == 0 then
                tasks ++ [ Subtask "new task" 1 ]

            else
                case taskAtIndex of
                    Just item ->
                        case item of
                            Task desc subtasks ->
                                setAt index (Task desc (addTask restOfPath subtasks)) tasks

                            Subtask description estimate ->
                                setAt index (Task description [ Subtask "new subtask" estimate ]) tasks

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
            , text ("Sum: " ++ String.fromInt (sumTasks model.tasks))
            ]
        ]
    }


taskView : List Int -> List Item -> Int -> Item -> Html Msg
taskView path parentTasks index item =
    case item of
        Subtask description days ->
            Html.li [ class "task" ]
                [ Html.div []
                    [ Html.span [] [ text description ]
                    , Html.code [] [ text (String.fromInt days) ]
                    , Html.button
                        [ style "border-radius" "100%"
                        , style "border" "1px solid black"
                        , style "width" "21px"
                        , style "height" "21px"
                        , onClick (AddTask (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    ]
                , if List.length parentTasks == index + 1 then
                    Html.button [ class "add", onClick (AddTask (path ++ [ index ])) ] [ text "add task" ]

                  else
                    text ""
                ]

        Task description subtasks ->
            Html.li [ class "task" ]
                [ Html.div []
                    [ Html.span [] [ text description ]
                    , Html.code [] [ text <| String.fromInt <| sumTasks subtasks ]
                    , Html.button
                        [ style "border-radius" "100%"
                        , style "border" "1px solid black"
                        , style "width" "21px"
                        , style "height" "21px"
                        , onClick (AddTask (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    ]
                , Html.ul [ class "subtasks" ] <| List.indexedMap (taskView (path ++ [ index ]) subtasks) subtasks
                , if List.length parentTasks == index + 1 then
                    Html.button [ class "add", onClick (AddTask (path ++ [ index ])) ] [ text "add task" ]

                  else
                    text ""
                ]


sumTasks items =
    List.foldl
        (\item sum ->
            case item of
                Subtask _ estimate ->
                    sum + estimate

                Task _ subtasks ->
                    sum + sumTasks subtasks
        )
        0
        items



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
