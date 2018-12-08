module Page.Search exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import RemoteData exposing (WebData)
import Route


type alias Image =
    { id : String
    , src : String
    , tags : List String
    }


type alias Model =
    { images : WebData (List Image)
    }


type Msg
    = ImageResponse (WebData (List Image))


decodeTag =
    D.string


decodeImage =
    D.map3 Image
        (D.field "id" D.string)
        (D.field "src" D.string)
        (D.field "tags" (D.list decodeTag))


decodeImages =
    D.list decodeImage


getImages : Cmd Msg
getImages =
    Http.get
        { url = "/api/search"
        , expect = Http.expectJson (RemoteData.fromResult >> ImageResponse) decodeImages
        }


init =
    ( Model RemoteData.Loading, getImages )


update msg model =
    case msg of
        ImageResponse response ->
            ( { model | images = response }
            , Cmd.none
            )


imageView image =
    div [ class "item" ]
        [ a [ Route.href (Route.ViewImage image.id) ] [ img [ class "item--image", src image.src ] [] ]
        , div [ class "item--tags" ] [ div [ class "tags are-small" ] (List.map (\tag -> span [ class "tag" ] [ text tag ]) image.tags) ]
        ]


view model =
    case model.images of
        RemoteData.NotAsked ->
            text "Initialising."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Failure err ->
            text ("Error: " ++ Debug.toString err)

        RemoteData.Success images ->
            if List.length images > 0 then
                div [ class "masonry" ] (List.map imageView images)
            else
                div [] [ text "No images to display" ]
