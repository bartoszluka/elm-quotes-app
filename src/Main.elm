module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { quote : Quote, favoritesList : List String }


type Quote
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading []
    , getNewQuote
    )



-- UPDATE


type Msg
    = GetNewQuote
    | GotQuote (Result Http.Error String)
    | AddFavorite String


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
            ( { model | favoritesList = favorite :: model.favoritesList }, Cmd.none )



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
        , favoritesList model
        ]


favoritesList : Model -> Html Msg
favoritesList model =
    case model.favoritesList of
        [] ->
            text "no favorites yet"

        list ->
            ul [] (List.map (\quote -> li [] [ text quote ]) list)


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
                , button [ onClick (AddFavorite quoteText), style "display" "block" ] [ text "add this quote to favorites" ]
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
