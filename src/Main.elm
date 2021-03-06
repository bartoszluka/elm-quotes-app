port module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Html exposing (Html)
import Http
import Json.Decode as D
import Json.Encode as E



-- MAIN


type alias QuotesList =
    { uid : Int
    , favoritesList : List Quote
    }


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
    { id : Int
    , content : String
    }


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
            ( model, getNewQuote )

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


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


view : Model -> Html Msg
view model =
    layout [ Background.color darkBackground ]
        (column
            [ Font.color white
            , centerX
            , spacing 40
            , paddingXY 150 100
            ]
            [ title
            , displayText model
            , getQuoteBtn
            , lazy favoritesList model
            ]
        )


title : Element Msg
title =
    el [ Font.size 40, centerX ] (text "Kanye's quotes")


gray : Color
gray =
    rgb255 216 222 233


lighterBg : Color
lighterBg =
    rgb255 76 86 106


white : Color
white =
    rgb255 236 239 244


darkBackground : Color
darkBackground =
    rgb255 46 52 64


darkBlue : Color
darkBlue =
    rgb255 94 129 172


grayBlue : Color
grayBlue =
    rgb255 129 161 193


lightBlue : Color
lightBlue =
    rgb255 136 192 208


mint : Color
mint =
    rgb255 143 188 187


displayText : Model -> Element Msg
displayText model =
    case model.quote of
        Failure ->
            el [] (text "Could not load a new quote :(")

        Loading ->
            el [] (text "Loading...")

        Success quoteText ->
            column
                [ spacing 20, centerX, width fill ]
                [ paragraph
                    [ height <| px 140
                    , Border.rounded 8
                    , Border.color gray
                    , Border.solid
                    , Border.width 1
                    , Font.size 29
                    , Font.italic
                    , padding 20
                    ]
                    [ text quoteText ]
                , addToFavoritesButton quoteText model
                ]


addToFavoritesButton : String -> Model -> Element Msg
addToFavoritesButton quoteText model =
    let
        btn : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
        btn attributes content =
            Input.button
                ([ rounded 7
                 , padding 15
                 , alignRight
                 ]
                    ++ attributes
                )
                content
    in
    if List.filter (\q -> quoteText == q.content) model.favoritesList |> List.isEmpty then
        btn
            [ Background.color lightBlue
            , Font.color darkBackground
            ]
            { onPress = Just (AddFavorite quoteText)
            , label = text "Add to favorites"
            }

    else
        btn
            [ Background.color lighterBg
            ]
            { onPress = Nothing
            , label = text "already added!"
            }


favoritesList : Model -> Element Msg
favoritesList model =
    case model.favoritesList of
        [] ->
            el [] (text "no favorites yet")

        list ->
            Keyed.column
                [ spacing 20
                , Border.width 1
                , Border.rounded 7
                ]
                (List.map displayKeyedQuote list)


displayKeyedQuote : Quote -> ( String, Element Msg )
displayKeyedQuote quote =
    ( String.fromInt quote.id, lazy displayQuote quote )


displayQuote : Quote -> Element Msg
displayQuote quote =
    row
        [ spacing 20
        , width fill
        , padding 20
        ]
        [ paragraph
            [ Border.rounded 4
            , Border.color gray
            , Border.solid
            , alignLeft
            ]
            [ text quote.content ]
        , Input.button
            [ Background.color <| rgb255 191 97 106
            , Font.color darkBackground
            , padding 12
            , rounded 7
            ]
            { onPress = Just (RemoveFromFavorites quote.id)
            , label = text "remove from favorites"
            }
        ]


getQuoteBtn : Element Msg
getQuoteBtn =
    Input.button
        [ Background.color darkBlue
        , alignLeft
        , rounded 7
        , padding 15
        , Font.size 30
        ]
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
        (D.field "content" D.string)



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
