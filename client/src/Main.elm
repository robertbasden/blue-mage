module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.AddFiles as AddFiles
import Page.Search as Search
import Page.Settings as Settings
import Page.ViewImage as ViewImage
import Route
import Url exposing (Url)


type Page
    = Search Search.Model
    | AddFiles AddFiles.Model
    | Settings
    | ViewImage String
    | NotFound


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , page : Page
    }


type Msg
    = NoOp
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | AddFilesMsg AddFiles.Msg
    | SearchMsg Search.Msg


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            Model url key NotFound
    in
    changeRouteTo (Route.fromUrl url) initialModel


changeRouteTo maybeRoute model =
    case maybeRoute of
        Just Route.Home ->
            let
                ( initialModel, initialCommand ) =
                    Search.init
            in
            ( { model | page = Search initialModel }, Cmd.map (\searchMsg -> SearchMsg searchMsg) initialCommand )

        Just Route.AddFiles ->
            ( { model | page = AddFiles AddFiles.init }, Cmd.none )

        Just (Route.ViewImage id) ->
            ( { model | page = ViewImage id }, Cmd.none )

        Just Route.Settings ->
            ( { model | page = Settings }, Cmd.none )

        _ ->
            ( { model | page = NotFound }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ChangedUrl url ->
            changeRouteTo (Route.fromUrl url) model

        SearchMsg subMsg ->
            case model.page of
                Search subModel ->
                    let
                        ( newModel, newCmd ) =
                            Search.update subMsg subModel
                    in
                    ( { model | page = Search newModel }, Cmd.map (\searchMsg -> SearchMsg searchMsg) newCmd )

                _ ->
                    ( model, Cmd.none )

        AddFilesMsg subMsg ->
            case model.page of
                AddFiles subModel ->
                    let
                        ( newModel, newCmd ) =
                            AddFiles.update subMsg subModel
                    in
                    ( { model | page = AddFiles newModel }, Cmd.map (\addFilesMsg -> AddFilesMsg addFilesMsg) newCmd )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


notifications =
    div [ class "notifications" ]
        [ notification
        ]


notification =
    div [ class "notification is-info" ]
        [ button [ class "delete" ]
            []
        , text "Primar lorem ipsum dolor sit amet, consectetur  adipiscing elit lorem ipsum dolor. Sit amet,  consectetur adipiscing elit"
        ]


navBar =
    nav [ attribute "aria-label" "main navigation", class "navbar is-primary is-fixed-top", attribute "role" "navigation" ]
        [ div [ class "navbar-menu", id "navbarBasicExample" ]
            [ div [ class "navbar-start" ]
                [ a [ class "navbar-item", Route.href Route.Home ]
                    [ text "Home" ]
                , a [ class "navbar-item", Route.href Route.AddFiles ]
                    [ text "Add Images" ]
                ]
            ]
        ]


breadCrumbs navItems =
    let
        navItem label =
            li []
                [ a [ href "#" ]
                    [ text label ]
                ]
    in
    nav [ class "breadcrumb" ]
        [ ul [] (List.map navItem navItems)
        ]


pageNotFound =
    div []
        [ h1 [ class "has-text-danger title is-4" ] [ text "Whoops! We could not find the page you are looking for!" ]
        ]


view : Model -> Document Msg
view model =
    { title = "BlueMage"
    , body =
        [ div []
            [ navBar
            , div [ class "section" ]
                [ div [ class "container is-fluid" ]
                    [ case model.page of
                        AddFiles subModel ->
                            Html.map (\subMsg -> AddFilesMsg subMsg) (AddFiles.view subModel)

                        Search subModel ->
                            Search.view subModel

                        Settings ->
                            Settings.view

                        ViewImage id ->
                            ViewImage.view id

                        _ ->
                            pageNotFound
                    ]
                ]
            ]
        ]
    }


subscriptions model =
    let
        uploadSubscriptions =
            case model.page of
                AddFiles subModel ->
                    AddFiles.getSubscriptions subModel
                        |> List.map
                            (\subSub ->
                                Sub.map (\subMsg -> AddFilesMsg subMsg) subSub
                            )

                _ ->
                    []
    in
    Sub.batch uploadSubscriptions


main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
