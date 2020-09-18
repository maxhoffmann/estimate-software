module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class, maxlength, style, value)
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
            [ Subtask "task" 1 ]
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
    case msg of
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
                tasks ++ [ Subtask "task" 1 ]

            else
                case taskAtIndex of
                    Just item ->
                        case item of
                            Task desc subtasks ->
                                setAt index (Task desc (addTask restOfPath subtasks)) tasks

                            Subtask description estimate ->
                                setAt index (Task description [ Subtask "task" estimate ]) tasks

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
                                setAt index
                                    (Task oldDescription
                                        (updateDescription
                                            restOfPath
                                            description
                                            subtasks
                                        )
                                    )
                                    tasks

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
                                setAt index
                                    (Task description
                                        (updateEstimate
                                            restOfPath
                                            estimate
                                            subtasks
                                        )
                                    )
                                    tasks

                            else
                                tasks

                        Subtask description _ ->
                            setAt index
                                (Subtask description
                                    (String.toInt estimate
                                        |> Maybe.withDefault 0
                                    )
                                )
                                tasks

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
            , if List.length model.tasks == 0 then
                Html.button
                    [ class "add"
                    , onClick (AddTask [ 0 ])
                    , style "cursor" "pointer"
                    ]
                    [ text "add" ]

              else
                text ""
            , Html.div [ style "border-top" "2px dashed lightgray", style "padding-top" "5px" ] [ text ("Sum: " ++ String.fromInt (sumTasks model.tasks)) ]
            ]
        ]
    }


taskView : List Int -> List Item -> Int -> Item -> Html Msg
taskView path parentTasks index item =
    case item of
        Subtask description days ->
            Html.li [ style "margin-bottom" "10px" ]
                [ Html.div []
                    [ Html.button
                        [ style "border" "1px solid black"
                        , style "width" "21px"
                        , style "height" "21px"
                        , onClick (AddTask (path ++ [ index, 0 ]))
                        , style "cursor" "pointer"
                        ]
                        [ text "+" ]
                    , Html.input
                        [ value (String.fromInt days)
                        , onInput
                            (UpdateEstimate (path ++ [ index ]))
                        , maxlength 3
                        , style "border" "0"
                        , style "font-family" "monospace"
                        , style "width" "30px"
                        , style "text-align" "center"
                        , style "padding" "0"
                        ]
                        []
                    , Html.input
                        [ value description
                        , onInput
                            (UpdateDescription (path ++ [ index ]))
                        , style "border" "0"
                        ]
                        []
                    ]
                , if List.length parentTasks == index + 1 then
                    Html.button
                        [ style "margin-top" "10px"
                        , style "cursor" "pointer"
                        , onClick
                            (AddTask
                                (path ++ [ index ])
                            )
                        ]
                        [ text "add" ]

                  else
                    text ""
                ]

        Task description subtasks ->
            Html.li [ style "margin-bottom" "20px" ]
                [ Html.div []
                    [ Html.button
                        [ style "border" "1px solid black"
                        , style "width" "21px"
                        , style "height" "21px"
                        , style "cursor" "pointer"
                        , onClick (AddTask (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    , Html.code
                        [ style "display" "inline-block"
                        , style "font-family" "monospace"
                        , style "width" "20px"
                        , style "text-align" "center"
                        , style "background-color" "lightgray"
                        , style "border-radius" "100%"
                        , style "cursor" "default"
                        , style "margin" "0 5px"
                        ]
                        [ text <| String.fromInt <| sumTasks subtasks ]
                    , Html.input
                        [ value description
                        , onInput
                            (UpdateDescription
                                (path ++ [ index ])
                            )
                        , style "border" "0"
                        ]
                        []
                    ]
                , Html.ul
                    [ style "list-style" "none"
                    , style "margin-top" "10px"
                    , style "padding-left" "22px"
                    ]
                  <|
                    List.indexedMap
                        (taskView (path ++ [ index ]) subtasks)
                        subtasks
                , if List.length parentTasks == index + 1 then
                    Html.button
                        [ class "add"
                        , onClick
                            (AddTask (path ++ [ index ]))
                        , style "cursor" "pointer"
                        ]
                        [ text "add" ]

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
