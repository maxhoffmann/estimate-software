module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class, classList, maxlength, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt, removeAt, setAt)



-- Model


type alias Model =
    { tasks : List Item
    , selectedTask : Path
    }


type alias Path =
    List Int


type Item
    = Task String (List Item)
    | Subtask String Int


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { tasks =
            [ Subtask "description" 1 ]
      , selectedTask = [ 0 ]
      }
    , Cmd.none
    )



-- Update


type Msg
    = AddItem Path
    | RemoveItem Path
    | UpdateDescription Path String
    | UpdateEstimate Path String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem path ->
            ( { model | tasks = addItem path model.tasks }, Cmd.none )

        RemoveItem path ->
            ( { model | tasks = removeItem path model.tasks }, Cmd.none )

        UpdateDescription path string ->
            ( { model | tasks = updateDescription path string model.tasks }, Cmd.none )

        UpdateEstimate path string ->
            ( { model | tasks = updateEstimate path string model.tasks }, Cmd.none )


addItem : Path -> List Item -> List Item
addItem path tasks =
    case path of
        index :: restOfPath ->
            let
                taskAtIndex =
                    getAt index tasks
            in
            if List.length restOfPath == 0 then
                tasks ++ [ Subtask "description" 1 ]

            else
                case taskAtIndex of
                    Just item ->
                        case item of
                            Task desc subtasks ->
                                setAt index (Task desc (addItem restOfPath subtasks)) tasks

                            Subtask description estimate ->
                                setAt index (Task description [ Subtask "description" estimate ]) tasks

                    Nothing ->
                        tasks

        [] ->
            tasks


removeItem : Path -> List Item -> List Item
removeItem path tasks =
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
                            case subtasks of
                                firstSubitem :: _ ->
                                    case firstSubitem of
                                        Task _ _ ->
                                            if List.length restOfPath > 0 then
                                                setAt index
                                                    (Task oldDescription
                                                        (removeItem
                                                            restOfPath
                                                            subtasks
                                                        )
                                                    )
                                                    tasks

                                            else
                                                setAt index (Subtask oldDescription (sumTasks subtasks)) tasks

                                        Subtask _ _ ->
                                            -- Convert Task to Subtask when deleting the last subtask
                                            if List.length restOfPath == 2 && List.length subtasks == 1 then
                                                setAt index (Subtask oldDescription (sumTasks subtasks)) tasks

                                            else if List.length restOfPath > 0 then
                                                setAt index
                                                    (Task oldDescription
                                                        (removeItem
                                                            restOfPath
                                                            subtasks
                                                        )
                                                    )
                                                    tasks

                                            else
                                                setAt index (Subtask oldDescription (sumTasks subtasks)) tasks

                                _ ->
                                    removeAt index tasks

                        Subtask _ _ ->
                            removeAt index tasks

                Nothing ->
                    tasks

        [] ->
            tasks


updateDescription : Path -> String -> List Item -> List Item
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


updateEstimate : Path -> String -> List Item -> List Item
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
        [ Html.main_ [ class "main" ]
            [ Html.ul [ class "top-level" ] <|
                (model.tasks
                    |> List.indexedMap (taskView model.selectedTask [] model.tasks)
                )
            , if List.length model.tasks == 0 then
                Html.button
                    [ class "button-add"
                    , onClick (AddItem [ 0 ])
                    ]
                    [ text "add" ]

              else
                text ""
            , Html.div [ class "sum" ] [ text ("Sum: " ++ String.fromInt (sumTasks model.tasks)) ]
            ]
        ]
    }


taskView : Path -> Path -> List Item -> Int -> Item -> Html Msg
taskView selectedPath path parentTasks index item =
    let
        currentPath =
            path ++ [ index ]
    in
    case item of
        Subtask description days ->
            Html.li [ classList [ ( "item", True ) ] ]
                [ Html.div [ classList [ ( "item--active", currentPath == selectedPath ) ] ]
                    [ Html.button
                        [ class "button"
                        , onClick
                            (RemoveItem
                                (currentPath ++ [ 0 ])
                            )
                        ]
                        [ text "-" ]
                    , Html.button
                        [ class "button"
                        , onClick (AddItem (currentPath ++ [ 0 ]))
                        ]
                        [ text "+" ]
                    , Html.input
                        [ value (String.fromInt days)
                        , onInput
                            (UpdateEstimate currentPath)
                        , maxlength 3
                        , class "estimate"
                        ]
                        []
                    , Html.input
                        [ value description
                        , onInput (UpdateDescription currentPath)
                        , class "description"
                        ]
                        []
                    ]
                , if List.length parentTasks == index + 1 then
                    Html.button
                        [ class "button-add"
                        , onClick (AddItem currentPath)
                        ]
                        [ text "add" ]

                  else
                    text ""
                ]

        Task description subtasks ->
            Html.li [ classList [ ( "item", True ) ] ]
                [ Html.div [ classList [ ( "item--active", currentPath == selectedPath ) ] ]
                    [ Html.button
                        [ class "button"
                        , onClick (RemoveItem currentPath)
                        ]
                        [ text "-" ]
                    , Html.button
                        [ class "button"
                        , onClick (AddItem (currentPath ++ [ 0 ]))
                        ]
                        [ text "+" ]
                    , Html.code
                        [ class "sum-subtasks"
                        ]
                        [ text <| String.fromInt <| sumTasks subtasks ]
                    , Html.input
                        [ value description
                        , onInput (UpdateDescription currentPath)
                        , class "description"
                        ]
                        []
                    ]
                , Html.ul
                    [ class "ul"
                    ]
                  <|
                    List.indexedMap
                        (taskView selectedPath currentPath subtasks)
                        subtasks
                , if List.length parentTasks == index + 1 then
                    Html.button
                        [ class "add"
                        , onClick
                            (AddItem currentPath)
                        , class "button-add"
                        ]
                        [ text "add" ]

                  else
                    text ""
                ]


sumTasks : List Item -> Int
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
