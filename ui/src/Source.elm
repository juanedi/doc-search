module Source exposing (Source, decoder, icon)

import Html.Styled
import Json.Decode
import Nri.Ui.Svg.V1 as NriSVG
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Source
    = Github
    | DropboxPaper


decoder : Json.Decode.Decoder Source
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "github" ->
                        Json.Decode.succeed Github

                    "dropbox-paper" ->
                        Json.Decode.succeed DropboxPaper

                    _ ->
                        Json.Decode.fail ("Unrecognized source: " ++ string)
            )


icon : Source -> NriSVG.Svg
icon source =
    (case source of
        Github ->
            svg
                [ width "100%"
                , height "100%"
                , viewBox "0 0 1024 1024"
                , fill "none"
                ]
                [ Svg.path
                    [ fillRule "evenodd"
                    , clipRule "evenodd"
                    , d "M8 0C3.58 0 0 3.58 0 8C0 11.54 2.29 14.53 5.47 15.59C5.87 15.66 6.02 15.42 6.02 15.21C6.02 15.02 6.01 14.39 6.01 13.72C4 14.09 3.48 13.23 3.32 12.78C3.23 12.55 2.84 11.84 2.5 11.65C2.22 11.5 1.82 11.13 2.49 11.12C3.12 11.11 3.57 11.7 3.72 11.94C4.44 13.15 5.59 12.81 6.05 12.6C6.12 12.08 6.33 11.73 6.56 11.53C4.78 11.33 2.92 10.64 2.92 7.58C2.92 6.71 3.23 5.99 3.74 5.43C3.66 5.23 3.38 4.41 3.82 3.31C3.82 3.31 4.49 3.1 6.02 4.13C6.66 3.95 7.34 3.86 8.02 3.86C8.7 3.86 9.38 3.95 10.02 4.13C11.55 3.09 12.22 3.31 12.22 3.31C12.66 4.41 12.38 5.23 12.3 5.43C12.81 5.99 13.12 6.7 13.12 7.58C13.12 10.65 11.25 11.33 9.47 11.53C9.76 11.78 10.01 12.26 10.01 13.01C10.01 14.08 10 14.94 10 15.21C10 15.42 10.15 15.67 10.55 15.59C13.71 14.53 16 11.53 16 8C16 3.58 12.42 0 8 0Z"
                    , transform "scale(64)"
                    , fill "#1B1F23"
                    ]
                    []
                ]

        DropboxPaper ->
            svg
                [ width "100%"
                , height "100%"
                , viewBox "0 0 178.81 187.43"
                , version "1.1"
                ]
                [ defs []
                    [ Svg.clipPath [ Svg.Attributes.id "clip1" ] [ Svg.path [ d "M 0 73 L 178.808594 73 L 178.808594 187.429688 L 0 187.429688 Z M 0 73 " ] [] ]
                    , Svg.clipPath [ Svg.Attributes.id "clip2" ] [ Svg.path [ d "M 0 0 L 178.808594 0 L 178.808594 114 L 0 114 Z M 0 0 " ] [] ]
                    ]
                , g [ id "surface1" ]
                    [ g [ Svg.Attributes.clipPath "url(#clip1)", clipRule "nonzero" ] [ Svg.path [ Svg.Attributes.style " stroke:none;fill-rule:nonzero;fill:rgb(70.599365%,81.599426%,90.19928%);fill-opacity:1;", d "M 0 130.472656 L 89.40625 187.429688 L 178.8125 130.472656 L 89.40625 73.519531 L 0 130.472656 " ] [] ]
                    , g [ Svg.Attributes.clipPath "url(#clip2)", clipRule "nonzero" ] [ Svg.path [ Svg.Attributes.style " stroke:none;fill-rule:nonzero;fill:rgb(25.099182%,39.99939%,68.998718%);fill-opacity:1;", d "M 0 56.949219 L 89.40625 113.90625 L 178.8125 56.949219 L 89.40625 -0.00390625 L 0 56.949219 " ] [] ]
                    ]
                ]
    )
        |> Html.Styled.fromUnstyled
        |> NriSVG.fromHtml
