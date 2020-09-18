module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput)
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
            [ Task "Test"
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
    | UpdateDescription (List Int) String
    | UpdateEstimate (List Int) String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        AddTask path ->
            ( { model | tasks = addTask path model.tasks }, Cmd.none )

        UpdateDescription path string ->
            ( { model | tasks = updateDescription path string model.tasks }, Cmd.none )

        UpdateEstimate path string ->
            ( { model | tasks = updateEstimate path string model.tasks }, Cmd.none )


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


updateDescription : List Int -> String -> List Item -> List Item
updateDescription path description tasks =
    case path of
        index :: restOfPath ->
            let
                itemAtIndex =
                    getAt index tasks
            in
            case itemAtIndex of
                Just item ->
                    case item of
                        Task oldDescription subtasks ->
                            if List.length restOfPath > 0 then
                                setAt index (Task oldDescription (updateDescription restOfPath description subtasks)) tasks

                            else
                                setAt index (Task description subtasks) tasks

                        Subtask _ estimate ->
                            setAt index (Subtask description estimate) tasks

                Nothing ->
                    tasks

        [] ->
            tasks


updateEstimate : List Int -> String -> List Item -> List Item
updateEstimate path estimate tasks =
    case path of
        index :: restOfPath ->
            let
                itemAtIndex =
                    getAt index tasks
            in
            case itemAtIndex of
                Just item ->
                    case item of
                        Task description subtasks ->
                            if List.length restOfPath > 0 then
                                setAt index (Task description (updateEstimate restOfPath estimate subtasks)) tasks

                            else
                                tasks

                        Subtask description _ ->
                            setAt index (Subtask description (String.toInt estimate |> Maybe.withDefault 0)) tasks

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
            [ Html.ul [ style "list-style" "none", style "padding" "0" ] <|
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
                    [ Html.button
                        [ style "border-radius" "100%"
                        , style "border" "1px solid black"
                        , style "width" "21px"
                        , style "height" "21px"
                        , onClick (AddTask (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    , Html.input [ value (String.fromInt days), onInput (UpdateEstimate (path ++ [ index ])) ] []
                    , Html.input [ value description, onInput (UpdateDescription (path ++ [ index ])) ] []
                    , text (String.fromInt index)
                    ]
                , if List.length parentTasks == index + 1 then
                    Html.button [ class "add", onClick (AddTask (path ++ [ index ])) ] [ text "add task" ]

                  else
                    text ""
                ]

        Task description subtasks ->
            Html.li [ class "task" ]
                [ Html.div []
                    [ Html.button
                        [ style "border-radius" "100%"
                        , style "border" "1px solid black"
                        , style "width" "21px"
                        , style "height" "21px"
                        , onClick (AddTask (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    , Html.code [] [ text <| String.fromInt <| sumTasks subtasks ]
                    , Html.input [ value description, onInput (UpdateDescription (path ++ [ index ])) ] []
                    , text (String.fromInt index)
                    ]
                , Html.ul [ style "list-style" "none" ] <| List.indexedMap (taskView (path ++ [ index ]) subtasks) subtasks
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
