module Main exposing (main)

import Browser
import Html
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Flags =
    ()


type Msg
    = SearchResult (Result Http.Error Decode.Value)


type alias Model =
    ()


{-|

    {
      "query": {
        "simple_query_string": {
          "query": <SEARCH_TERM>,
          "fields": ["contents"]
        }
      },
      "highlight":{
          "fragment_size":50,
          "fields":{
             "contents":{}
          }
       },
      "fields": ["url"],
      "_source": false
    }

-}
exampleQuery : String -> Encode.Value
exampleQuery searchTerm =
    Encode.object
        [ ( "query"
          , Encode.object
                [ ( "simple_query_string"
                  , Encode.object
                        [ ( "query", Encode.string searchTerm )
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
    ( ()
    , let
        jsonQuery =
            exampleQuery "migration"
      in
      Http.post
        { url = "http://localhost:9200/doc-search/_search"
        , body = Http.jsonBody jsonQuery
        , expect = Http.expectJson SearchResult Decode.value
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Doc Search"
    , body =
        [ Html.text "Hello!" ]
    }
