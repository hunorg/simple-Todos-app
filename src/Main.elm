module Main exposing (..)

import Array
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import String exposing (dropRight)


type alias Model =
    { todos : List Todo
    , newTodo : String
    , selectedTodo : Maybe Todo
    }


type alias Todo =
    { description : String
    , completed : Bool
    , editing : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = []
      , newTodo = ""
      , selectedTodo = Nothing
      }
    , Cmd.none
    )


type Msg
    = AddTodo
    | UpdateNewTodo String
    | SelectTodo Int
    | ToggleTodo
    | UpdateSelectedTodo Todo
    | EditTodo Int
    | RemoveTodo
    | RemoveCompleted
    | Clear
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            ( { model
                | todos = model.todos ++ [ { description = model.newTodo, completed = False, editing = False } ]
                , newTodo = ""
                , selectedTodo = Nothing
              }
            , Cmd.none
            )

        UpdateNewTodo newDescription ->
            ( { model | newTodo = newDescription }, Cmd.none )

        SelectTodo index ->
            if model.selectedTodo == Nothing then
                ( { model | selectedTodo = Array.get index (Array.fromList model.todos) }, Cmd.none )

            else
                ( { model | selectedTodo = Nothing }, Cmd.none )

        ToggleTodo ->
            case model.selectedTodo of
                Just todo ->
                    let
                        newTodo =
                            if todo.completed then
                                { todo | description = dropRight 12 todo.description, completed = False }

                            else
                                { todo | description = todo.description ++ " (completed)", completed = True }

                        newTodos =
                            List.map
                                (\t ->
                                    if t == todo then
                                        newTodo

                                    else
                                        t
                                )
                                model.todos
                    in
                    ( { model | todos = newTodos, selectedTodo = Just newTodo }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSelectedTodo updatedTodo ->
            let
                newTodos =
                    List.map
                        (\t ->
                            if t == Maybe.withDefault { description = "", completed = False, editing = False } model.selectedTodo then
                                updatedTodo

                            else
                                t
                        )
                        model.todos
            in
            ( { model | todos = newTodos, selectedTodo = Just updatedTodo }, Cmd.none )

        EditTodo index ->
            let
                selectedTodo =
                    Array.get index (Array.fromList model.todos)

                updatedTodo =
                    Maybe.withDefault { description = "", completed = False, editing = False } selectedTodo

                newTodos =
                    List.map
                        (\t ->
                            if t == updatedTodo then
                                { updatedTodo | editing = True }

                            else
                                t
                        )
                        model.todos
            in
            ( { model | todos = newTodos, selectedTodo = Just updatedTodo }, Cmd.none )

        RemoveTodo ->
            case model.selectedTodo of
                Just todo ->
                    let
                        newTodos =
                            List.filter (\t -> t /= todo) model.todos
                    in
                    ( { model | todos = newTodos, selectedTodo = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RemoveCompleted ->
            let
                newTodos =
                    List.filter (\t -> not t.completed) model.todos
            in
            ( { model | todos = newTodos }, Cmd.none )

        Clear ->
            ( { model | todos = [] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.newTodo, onInput UpdateNewTodo, placeholder "Add a new todo item here." ] []
        , button [ onClick AddTodo ] [ text "Add Todo" ]
        , ul []
            (List.indexedMap
                (\index todo ->
                    li [ onClick (SelectTodo index) ]
                        [ if todo.editing then
                            div []
                                [ input [ value todo.description, onInput (\newDescription -> UpdateSelectedTodo { todo | description = newDescription }) ] []
                                , button [ onClick (UpdateSelectedTodo { todo | editing = False }) ] [ text "Save" ]
                                ]

                          else
                            span []
                                [ text todo.description
                                , if model.selectedTodo == Just todo then
                                    div []
                                        [ button [ onClick (EditTodo index) ] [ text "Edit" ]
                                        , button [ onClick ToggleTodo ] [ text "Completed" ]
                                        , button [ onClick RemoveTodo ] [ text "Remove" ]
                                        ]

                                  else
                                    text ""
                                ]
                        ]
                )
                model.todos
            )
        , button [ onClick RemoveCompleted ] [ text "Remove Completed" ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]


subscriptions : a -> Sub Msg
subscriptions _ =
    onKeyPress (Decode.map submitKey keyDecoder)


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


submitKey : String -> Msg
submitKey key =
    if key == "Enter" then
        AddTodo

    else
        NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

