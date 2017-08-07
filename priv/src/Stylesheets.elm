port module Stylesheets exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (namespace)
import Css.File exposing (CssFileStructure, CssCompilerProgram)


type Tileset
    = White
    | Black


type Class
    = Tile String
    | Tileset Tileset


riichi : Stylesheet
riichi =
    (stylesheet << namespace "riichi") <|
        List.concat
            [ List.map tile tiles
            , List.map (tileset White) tiles
            , List.map (tileset Black) tiles
            ]


applyTileset : Tileset -> String -> Style
applyTileset set tile =
    batch
        [ backgroundImage (tileImage set "Front")
        , children
            [ span
                [ backgroundImage (tileImage set tile) ]
            ]
        ]


tile : String -> Snippet
tile tile =
    class (Tile tile)
        [ display inlineBlock
        , applyTileset White tile
        , margin (px 10)
        , width (px 60)
        , height (px 80)
        , backgroundSize (pct 100)
        , children
            [ span
                [ display block
                , height (pct 100)
                , width (pct 100)
                , backgroundSize (pct 80)
                , backgroundRepeat noRepeat
                , backgroundPosition center
                ]
            ]
        ]


tileset : Tileset -> String -> Snippet
tileset set tile =
    class (Tileset set)
        [ descendants
            [ class (Tile tile)
                [ applyTileset set tile ]
            ]
        ]


tiles : List String
tiles =
    List.concat
        [ [ "Chun"
          , "Hatsu"
          , "Haku"
          , "Ton"
          , "Nan"
          , "Shaa"
          , "Pei"
          ]
        , List.range 1 9 |> List.map toString |> List.map (String.append "Pin")
        , List.range 1 9 |> List.map toString |> List.map (String.append "Sou")
        , List.range 1 9 |> List.map toString |> List.map (String.append "Man")
        ]


tileImage : Tileset -> String -> BackgroundImage {}
tileImage set tile =
    url (String.concat [ "../images/", toString set, "/", tile, ".png" ])


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "riichi.css", Css.File.compile [ riichi ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
