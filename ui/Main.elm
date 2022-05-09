module Main exposing (main)

import Browser
import Html


type alias Flags =
    ()


type alias Msg =
    ()


type alias Model =
    ()


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
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Doc Search"
    , body =
        [ Html.text "Hello!" ]
    }
