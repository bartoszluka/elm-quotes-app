port module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html exposing (Html)
import Http
import Json.Decode as D
import Json.Encode as E



-- MAIN


type alias QuotesList =
    { uid : Int, favoritesList : List Quote }


main : Program E.Value Model Msg
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
    { id : Int, content : String }


type FetchedQuote
    = Failure
    | Loading
    | Success String


emptyModel : Model
emptyModel =
    { quote = Loading
    , favoritesList = []
    , uid = 0
    }


maybeStorage : Maybe { uid : Int, favoritesList : List Quote } -> Maybe Model
maybeStorage maybeModel =
    Maybe.map (\model -> Model Loading model.favoritesList model.uid) maybeModel


init : E.Value -> ( Model, Cmd Msg )
init storageModel =
    ( case D.decodeValue storageDecoder storageModel of
        Ok model ->
            Model Loading model.favoritesList model.uid

        Err _ ->
            emptyModel
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
    layout []
        (column []
            [ displayText model
            , getQuoteBtn
            , lazy favoritesList model
            ]
        )


displayText : Model -> Element Msg
displayText model =
    case model.quote of
        Failure ->
            el [] (text "Could not load a new quote :(")

        Loading ->
            el [] (text "Loading...")

        Success quoteText ->
            row []
                [ text quoteText
                , if List.filter (\q -> quoteText == q.content) model.favoritesList |> List.isEmpty then
                    Input.button []
                        { onPress = Just (AddFavorite quoteText)
                        , label = text "add this quote to favorites"
                        }

                  else
                    let
                        grey =
                            Element.rgb 80 80 80
                    in
                    Input.button
                        [ Background.color grey
                        , Region.description
                            "A publish date is required before saving a blogpost."
                        ]
                        { onPress = Nothing
                        , label = text "this quote has been already added!"
                        }
                ]


favoritesList : Model -> Element Msg
favoritesList model =
    case model.favoritesList of
        [] ->
            text "no favorites yet"

        list ->
            Keyed.column [] (List.map displayKeyedQuote list)


displayKeyedQuote : Quote -> ( String, Element Msg )
displayKeyedQuote quote =
    ( String.fromInt quote.id, lazy displayQuote quote )


displayQuote : Quote -> Element Msg
displayQuote quote =
    column []
        [ text <| String.fromInt quote.id ++ ": " ++ quote.content
        , Input.button []
            { onPress = Just (RemoveFromFavorites quote.id)
            , label = text "remove from favorites"
            }
        ]


getQuoteBtn : Element Msg
getQuoteBtn =
    Input.button []
        { onPress = Just GetNewQuote
        , label = text "get new quote"
        }


storageDecoder : D.Decoder QuotesList
storageDecoder =
    D.map2 QuotesList
        (D.field "uid" D.int)
        (D.field "favoritesList" (D.list decodeQuote))


decodeQuote : D.Decoder Quote
decodeQuote =
    D.map2 Quote
        (D.field "id" D.int)
        (D.field "quote" D.string)



-- HTTP


getNewQuote : Cmd Msg
getNewQuote =
    Http.get
        { url = "https://api.kanye.rest"
        , expect = Http.expectJson GotQuote quoteDecoder
        }


quoteDecoder : D.Decoder String
quoteDecoder =
    D.field "quote" D.string
