module Main exposing (main)

import Browser
import Css
import Html.Parser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Logo.V1 as Logo
import Nri.Ui.Message.V3 as Message
import Nri.Ui.Shadows.V1 as Shadows
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.TextInput.V7 as TextInput
import Nri.Ui.UiIcon.V1 as UiIcon
import Source exposing (Source)


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
    { name : String
    , source : Source
    , url : String
    , highlights : List String
    }


responseDecoder : Decode.Decoder (List Match)
responseDecoder =
    Decode.at [ "hits", "hits" ] (Decode.list matchDecoder)


matchDecoder : Decode.Decoder Match
matchDecoder =
    let
        decodeSingletonList itemDecoder =
            -- FIXME: i think we shouldn't need to handle these singleton lists
            -- once we explicitly define the ES mapping.
            Decode.andThen
                (\values ->
                    case values of
                        [ value ] ->
                            Decode.succeed value

                        _ ->
                            Decode.fail "Expected to see a list with exactly one element"
                )
                (Decode.list itemDecoder)
    in
    Decode.succeed Match
        |> DecodePipeline.requiredAt [ "fields", "name" ] (decodeSingletonList Decode.string)
        |> DecodePipeline.requiredAt [ "fields", "source" ] (decodeSingletonList Source.decoder)
        |> DecodePipeline.requiredAt [ "fields", "url" ] (decodeSingletonList Decode.string)
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
                                , ( "fields", Encode.list Encode.string [ "name", "contents" ] )
                                ]
                          )
                        ]
                  )
                , ( "highlight"
                  , Encode.object
                        [ ( "fragment_size", Encode.int 300 )
                        , ( "encoder", Encode.string "html" )
                        , ( "fields", Encode.object [ ( "contents", Encode.object [] ) ] )
                        ]
                  )
                , ( "size", Encode.int 100 )
                , ( "fields", Encode.list Encode.string [ "url", "source", "name" ] )
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
            , Shadows.low
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
    Html.div [ css [ Css.padding (Css.px 18) ] ]
        [ Message.view
            [ Message.plaintext (String.fromInt (List.length model.matches) ++ " results")
            , Message.tiny
            , Message.icon UiIcon.searchInCicle
            , Message.success
            ]
        , Html.ul
            [ css
                [ Css.padding Css.zero
                , Css.listStyle Css.none
                ]
            ]
            (List.map viewMatch model.matches)
        ]


viewMatch : Match -> Html Msg
viewMatch match =
    Html.li
        [ css
            [ Css.marginBottom (Css.px 20)
            , Css.displayFlex
            ]
        ]
        [ match.source
            |> Source.icon
            |> Svg.withHeight (Css.px 20)
            |> Svg.withCss
                [ Css.marginRight (Css.px 7)
                , Css.paddingTop (Css.px 6)
                ]
            |> Svg.toHtml
        , Html.div []
            (List.append
                [ Html.a
                    [ Html.Styled.Attributes.href match.url
                    , Html.Styled.Attributes.target "_blank"
                    , css
                        [ Css.color Css.inherit
                        , Css.textDecoration Css.inherit
                        , Css.hover [ Css.textDecoration Css.underline ]
                        ]
                    ]
                    [ Heading.h3 [] [ Html.text match.name ]
                    ]
                , Html.div
                    [ css
                        [ Fonts.baseFont
                        , Css.fontSize (Css.px 12)
                        ]
                    ]
                    [ Html.text match.url ]
                ]
                (match.highlights
                    |> List.head
                    |> Maybe.map (\highlight -> [ viewHighlight highlight ])
                    |> Maybe.withDefault []
                )
            )
        ]


viewHighlight : String -> Html msg
viewHighlight highlight =
    let
        viewNode node =
            case node of
                Html.Parser.Text text ->
                    Html.text text

                Html.Parser.Element tag _ children ->
                    if tag == "em" then
                        Html.em
                            [ css
                                [ Css.backgroundColor Colors.highlightYellow
                                , Css.fontWeight Css.bold
                                , Css.padding (Css.px 4)
                                , Css.borderRadius (Css.px 6)
                                ]
                            ]
                            (List.map viewNode children)

                    else
                        Html.text ""

                Html.Parser.Comment _ ->
                    Html.text ""
    in
    case Html.Parser.run highlight of
        Err _ ->
            Html.text ""

        Ok nodes ->
            Html.div
                [ css
                    [ Css.lineHeight (Css.px 18)
                    , Fonts.baseFont
                    , Css.color Colors.gray45
                    , Css.fontSize (Css.px 10)
                    , Css.marginTop (Css.px 10)
                    , Css.maxWidth (Css.px 500)
                    ]
                ]
                (List.map viewNode nodes)
