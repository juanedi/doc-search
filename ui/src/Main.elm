module Main exposing (main)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Logo.V1 as Logo
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.TextInput.V7 as TextInput


type alias Flags =
    ()


type Msg
    = InputChanged String
    | TriggerSearch
    | SearchResult (Result Http.Error (List Match))


type alias Model =
    { input : String
    , queryState : QueryState
    , matches : List Match
    }


type QueryState
    = Idle
    | Asking String
    | Failed Http.Error


type alias Match =
    { url : String
    , highlights : List String
    }


responseDecoder : Decode.Decoder (List Match)
responseDecoder =
    Decode.at [ "hits", "hits" ] (Decode.list matchDecoder)


matchDecoder : Decode.Decoder Match
matchDecoder =
    Decode.succeed Match
        |> DecodePipeline.requiredAt [ "fields", "url" ]
            (Decode.andThen
                (\values ->
                    case values of
                        [ url ] ->
                            Decode.succeed url

                        _ ->
                            Decode.fail "Expected to see a list with exactly one element"
                )
                (Decode.list Decode.string)
            )
        |> DecodePipeline.requiredAt [ "highlight", "contents" ] (Decode.list Decode.string)


main : Program Flags Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { input = ""
      , queryState = Idle
      , matches = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )

        TriggerSearch ->
            ( { model | queryState = Asking model.input }
            , triggerSearch model.input
            )

        SearchResult result ->
            ( case result of
                Err error ->
                    { model | queryState = Failed error }

                Ok matches ->
                    { model | queryState = Idle, matches = matches }
            , Cmd.none
            )


triggerSearch : String -> Cmd Msg
triggerSearch input =
    let
        jsonQuery =
            Encode.object
                [ ( "query"
                  , Encode.object
                        [ ( "simple_query_string"
                          , Encode.object
                                [ ( "query", Encode.string input )
                                , ( "fields", Encode.list Encode.string [ "contents" ] )
                                ]
                          )
                        ]
                  )
                , ( "highlight"
                  , Encode.object
                        [ ( "fragment_size", Encode.int 50 )
                        , ( "fields", Encode.object [ ( "contents", Encode.object [] ) ] )
                        ]
                  )
                , ( "fields", Encode.list Encode.string [ "url" ] )
                , ( "_source", Encode.bool False )
                ]
    in
    Http.post
        { url = "http://localhost:9200/doc-search/_search"
        , body = Http.jsonBody jsonQuery
        , expect = Http.expectJson SearchResult responseDecoder
        }


view : Model -> Browser.Document Msg
view model =
    { title = "Doc Search"
    , body =
        List.map Html.toUnstyled
            [ case model.matches of
                [] ->
                    viewLanding model

                _ ->
                    viewMatchesPage model
            ]
    }


viewLanding : Model -> Html Msg
viewLanding model =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.paddingTop (Css.pct 20)
            , Css.height (Css.vh 100)
            ]
        ]
        [ Logo.noredink
            |> Svg.withWidth (Css.px 400)
            |> Svg.withCss [ Css.marginBottom (Css.px 40) ]
            |> Svg.toHtml
        , Html.form
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                ]
            , Events.onSubmit TriggerSearch
            ]
            [ TextInput.view ""
                (List.concat
                    [ [ TextInput.search InputChanged
                      , TextInput.value model.input
                      , TextInput.autofocus
                      , TextInput.css [ Css.minWidth (Css.px 700) ]
                      , TextInput.placeholder "Search anywhere"
                      ]
                    , case model.queryState of
                        Asking _ ->
                            [ TextInput.loading ]

                        _ ->
                            []
                    ]
                )
            ]
        ]


viewMatchesPage : Model -> Html Msg
viewMatchesPage model =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        [ viewMatchesPageHeader model
        , viewMatches model
        ]


viewMatchesPageHeader : Model -> Html Msg
viewMatchesPageHeader model =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.padding (Css.px 18)
            , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray85
            ]
        ]
        [ Logo.noredink
            |> Svg.withWidth (Css.px 150)
            |> Svg.withCss [ Css.marginTop (Css.px 9) ]
            |> Svg.toHtml
        , Html.form [ Events.onSubmit TriggerSearch ]
            [ TextInput.view ""
                (List.concat
                    [ [ TextInput.search InputChanged
                      , TextInput.value model.input
                      , TextInput.autofocus
                      , TextInput.css
                            [ Css.minWidth (Css.px 400)
                            , Css.marginLeft (Css.px 30)
                            ]
                      ]
                    , case model.queryState of
                        Asking _ ->
                            [ TextInput.loading ]

                        _ ->
                            []
                    ]
                )
            ]
        ]


viewMatches : Model -> Html Msg
viewMatches model =
    let
        viewMatch match =
            Html.li [ css [ Css.marginBottom (Css.px 15) ] ]
                [ Heading.h3 [] [ Html.text match.url ]
                , case match.highlights of
                    [] ->
                        Html.text ""

                    -- [ highlight ] ->
                    --     viewHighlight highlight
                    highlight :: _ ->
                        viewHighlight highlight
                ]

        viewHighlight highlight =
            -- TODO: render markdown?
            Html.text highlight
    in
    Html.div [ css [ Css.padding2 Css.zero (Css.px 18) ] ]
        [ Html.div [] [ Html.text (String.fromInt (List.length model.matches) ++ " matches found") ]
        , Html.ul
            [ css
                [ Css.padding Css.zero
                , Css.listStyle Css.none
                ]
            ]
            (List.map viewMatch model.matches)
        ]
