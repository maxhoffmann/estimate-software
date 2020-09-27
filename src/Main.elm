module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class, maxlength, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt, removeAt, setAt)



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
            [ Subtask "description" 1 ]
      }
    , Cmd.none
    )



-- Update


type Msg
    = AddItem (List Int)
    | RemoveItem (List Int)
    | UpdateDescription (List Int) String
    | UpdateEstimate (List Int) String


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


addItem : List Int -> List Item -> List Item
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


removeItem : List Int -> List Item -> List Item
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
                            if List.length restOfPath > 0 then
                                -- convert Task to Subtask if last subtask is deleted
                                if List.length restOfPath == 1 && List.length subtasks == 1 then
                                    setAt index
                                        (Subtask oldDescription
                                            (sumTasks subtasks)
                                        )
                                        tasks

                                else
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
                            removeAt index tasks

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
        [ Html.main_ [ class "main" ]
            [ Html.ul [ class "top-level" ] <|
                (model.tasks
                    |> List.indexedMap (taskView [] model.tasks)
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


taskView : List Int -> List Item -> Int -> Item -> Html Msg
taskView path parentTasks index item =
    case item of
        Subtask description days ->
            Html.li [ class "subtask" ]
                [ Html.div []
                    [ Html.button
                        [ class "button"
                        , onClick (RemoveItem (path ++ [ index, 0 ]))
                        ]
                        [ text "-" ]
                    , Html.button
                        [ class "button"
                        , onClick (AddItem (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    , Html.input
                        [ value (String.fromInt days)
                        , onInput
                            (UpdateEstimate (path ++ [ index ]))
                        , maxlength 3
                        , class "estimate"
                        ]
                        []
                    , Html.input
                        [ value description
                        , onInput
                            (UpdateDescription (path ++ [ index ]))
                        , class "description"
                        ]
                        []
                    ]
                , if List.length parentTasks == index + 1 then
                    Html.button
                        [ class "button-add"
                        , onClick
                            (AddItem
                                (path ++ [ index ])
                            )
                        ]
                        [ text "add" ]

                  else
                    text ""
                ]

        Task description subtasks ->
            Html.li [ class "subtask" ]
                [ Html.div []
                    [ Html.button
                        [ class "button"
                        , onClick (RemoveItem (path ++ [ index ]))
                        ]
                        [ text "-" ]
                    , Html.button
                        [ class "button"
                        , onClick (AddItem (path ++ [ index, 0 ]))
                        ]
                        [ text "+" ]
                    , Html.code
                        [ class "sum-subtasks"
                        ]
                        [ text <| String.fromInt <| sumTasks subtasks ]
                    , Html.input
                        [ value description
                        , onInput
                            (UpdateDescription
                                (path ++ [ index ])
                            )
                        , class "description"
                        ]
                        []
                    ]
                , Html.ul
                    [ class "ul"
                    ]
                  <|
                    List.indexedMap
                        (taskView (path ++ [ index ]) subtasks)
                        subtasks
                , if List.length parentTasks == index + 1 then
                    Html.button
                        [ class "add"
                        , onClick
                            (AddItem (path ++ [ index ]))
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
