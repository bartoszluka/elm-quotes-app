port module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode as E



-- MAIN


main : Program (Maybe { uid : Int, favoritesList : List Quote }) Model Msg
main =
    Browser.document
        { init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = \model -> { title = "Elm • Kanye's quotes", body = [ view model ] }
        }


port saveToStorage : E.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ saveToStorage (encodeModel newModel), cmds ]
    )



-- MODEL


type alias Model =
    { quote : FetchedQuote
    , favoritesList : List Quote
    , uid : Int
    }


type alias Quote =
    { id : Int
    , content : String
    }


type FetchedQuote
    = Failure
    | Loading
    | Success String


emptyModel : Model
emptyModel =
    { quote = Loading, favoritesList = [], uid = 0 }


maybeStorage : Maybe { uid : Int, favoritesList : List Quote } -> Maybe Model
maybeStorage maybeModel =
    Maybe.map (\model -> Model Loading model.favoritesList model.uid) maybeModel


init : Maybe { uid : Int, favoritesList : List Quote } -> ( Model, Cmd Msg )
init storageModel =
    ( Maybe.withDefault emptyModel (maybeStorage storageModel)
    , getNewQuote
    )


type Msg
    = GetNewQuote
    | GotQuote (Result Http.Error String)
    | AddFavorite String
    | RemoveFromFavorites Int


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "uid", E.int model.uid )
        , ( "favoritesList", E.list encodeQuote model.favoritesList )
        ]


encodeQuote : Quote -> E.Value
encodeQuote quote =
    E.object [ ( "id", E.int quote.id ), ( "content", E.string quote.content ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewQuote ->
            ( { model | quote = Loading }, getNewQuote )

        GotQuote result ->
            case result of
                Ok url ->
                    ( { model | quote = Success url }, Cmd.none )

                Err _ ->
                    ( { model | quote = Failure }, Cmd.none )

        AddFavorite favorite ->
            ( { model | favoritesList = Quote model.uid favorite :: model.favoritesList, uid = model.uid + 1 }, Cmd.none )

        RemoveFromFavorites id ->
            ( { model | favoritesList = List.filter (\fav -> fav.id /= id) model.favoritesList }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ displayText model
        , getQuoteBtn
        , lazy favoritesList model
        ]


favoritesList : Model -> Html Msg
favoritesList model =
    case model.favoritesList of
        [] ->
            text "no favorites yet"

        list ->
            Keyed.ul [] (List.map displayKeyedQuote list)


displayKeyedQuote : Quote -> ( String, Html Msg )
displayKeyedQuote quote =
    ( String.fromInt quote.id, lazy displayQuote quote )


displayQuote : Quote -> Html Msg
displayQuote quote =
    li []
        [ text <| String.fromInt quote.id ++ ": " ++ quote.content
        , button [ onClick (RemoveFromFavorites quote.id) ] [ text "remove from favorites" ]
        ]


displayText : Model -> Html Msg
displayText model =
    case model.quote of
        Failure ->
            div []
                [ text "Could not load a new quote :(" ]

        Loading ->
            div []
                [ text "Loading..." ]

        Success quoteText ->
            div []
                [ text quoteText
                , if List.filter (\q -> quoteText == q.content) model.favoritesList |> List.isEmpty then
                    button [ onClick (AddFavorite quoteText), style "display" "block" ] [ text "add this quote to favorites" ]

                  else
                    button [ style "display" "block", attribute "disabled" "" ] [ text "this quote is already added!" ]
                ]


getQuoteBtn : Html Msg
getQuoteBtn =
    button [ onClick GetNewQuote, style "display" "block" ]
        [ text "get new quote"
        ]



-- HTTP


getNewQuote : Cmd Msg
getNewQuote =
    Http.get
        { url = "https://api.kanye.rest"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : Decoder String
quoteDecoder =
    field "quote" string
