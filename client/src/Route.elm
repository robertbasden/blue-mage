module Route exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | AddFiles
    | ViewImage String
    | Settings


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map AddFiles (s "add-files")
        , Parser.map ViewImage (s "image" </> string)
        , Parser.map Settings (s "settings")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "#/"

        AddFiles ->
            "#/add-files"

        ViewImage id ->
            "#/image/" ++ id

        Settings ->
            "#/settings"
