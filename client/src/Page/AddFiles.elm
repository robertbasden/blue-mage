module Page.AddFiles exposing (..)

import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Random
import Task
import Uuid


type alias FileUpload =
    { id : String
    , file : File
    , previewUrl : Maybe String
    , progress : FileUploadProgress
    }


type FileUploadProgress
    = Waiting
    | InProgress Int
    | Complete
    | Failed


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotPreview ( String, String )
    | GotProgress String Http.Progress
    | Uploaded (Result Http.Error ())


type alias Model =
    { imageUploads : List FileUpload
    , hover : Bool
    , seed : Random.Seed
    }


init =
    Model [] False (Random.initialSeed 1)


getSubscriptions model =
    List.map
        (\imageUpload ->
            Http.track ("file-upload-progress-" ++ imageUpload.id) (GotProgress imageUpload.id)
        )
        model.imageUploads


update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "image/*" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file files ->
            let
                allFiles =
                    file :: files

                ( newUploads, nextSeed ) =
                    List.foldl
                        (\f ( uploads, seed ) ->
                            let
                                ( uuid, newSeed ) =
                                    Random.step Uuid.uuidGenerator seed

                                fileUpload =
                                    FileUpload (Uuid.toString uuid) f Nothing Waiting
                            in
                            ( uploads ++ [ fileUpload ], newSeed )
                        )
                        ( [], model.seed )
                        allFiles

                previewTasks =
                    List.map
                        (\upload ->
                            Task.map (\t -> ( upload.id, t )) (File.toUrl upload.file)
                        )
                        newUploads
                        |> List.map (\task -> Task.perform GotPreview task)

                uploadTasks =
                    List.map
                        (\upload ->
                            Http.request
                                { method = "POST"
                                , url = "/api/image"
                                , headers = []
                                , body = Http.multipartBody (List.map (Http.filePart "files[]") [ upload.file ])
                                , expect = Http.expectWhatever Uploaded
                                , timeout = Nothing
                                , tracker = Just ("file-upload-progress-" ++ upload.id)
                                }
                        )
                        newUploads

                uploadTask =
                    Http.request
                        { method = "POST"
                        , url = "/api/image"
                        , headers = []
                        , body = Http.multipartBody (List.map (Http.filePart "files[]") allFiles)
                        , expect = Http.expectWhatever Uploaded
                        , timeout = Nothing
                        , tracker = Just "upload"
                        }
            in
            ( { model
                | imageUploads = model.imageUploads ++ newUploads
                , hover = False
                , seed = nextSeed
              }
            , Cmd.batch (previewTasks ++ uploadTasks)
            )

        GotPreview ( id, previewUrl ) ->
            ( { model
                | imageUploads =
                    List.map
                        (\imageUpload ->
                            if imageUpload.id == id then
                                { imageUpload | previewUrl = Just previewUrl }
                            else
                                imageUpload
                        )
                        model.imageUploads
              }
            , Cmd.none
            )

        GotProgress id progress ->
            case progress of
                Http.Sending p ->
                    ( { model
                        | imageUploads =
                            List.map
                                (\imageUpload ->
                                    if imageUpload.id == id then
                                        if Http.fractionSent p == 1 then
                                            { imageUpload | progress = Complete }
                                        else
                                            { imageUpload | progress = InProgress (Basics.floor (Http.fractionSent p * 100)) }
                                    else
                                        imageUpload
                                )
                                model.imageUploads
                      }
                    , Cmd.none
                    )

                Http.Receiving _ ->
                    ( model, Cmd.none )

        Uploaded result ->
            ( model, Cmd.none )


previewImage maybePreviewImage =
    case maybePreviewImage of
        Just previewUrl ->
            figure [ class "image is-64x64" ]
                [ img [ src previewUrl ]
                    []
                ]

        Nothing ->
            figure [ class "image is-64x64" ]
                []


mediaObject left right =
    article [ class "media" ]
        [ figure [ class "media-left" ] left
        , div [ class "media-content" ] right
        ]


progressBar progress_ =
    progress [ class "progress is-primary is-small", Html.Attributes.max "100", value (String.fromInt progress_) ]
        [ text (String.fromInt progress_ ++ "%") ]


displayFileSize fileSizeInBytes =
    if fileSizeInBytes < 100000 then
        String.fromInt (Basics.round (toFloat fileSizeInBytes / 1000)) ++ "kb"
    else
        String.fromFloat (toFloat (Basics.round (toFloat fileSizeInBytes / 100000)) / 10) ++ "MB"


imageUploadWaiting id name size previewUrl =
    div [ class "image-upload image-upload--waiting" ]
        [ mediaObject
            [ previewImage previewUrl
            ]
            [ h1 [ class "title is-5" ] [ text name ]
            , h2 [ class "subtitle is-6" ] [ text (displayFileSize size) ]
            , h2 [ class "subtitle is-6" ] [ text "Waiting..." ]
            ]
        ]


imageUploadInProgress id name size previewUrl progress =
    div [ class "image-upload image-upload--in-progress" ]
        [ mediaObject
            [ previewImage previewUrl
            ]
            [ h1 [ class "title is-5" ] [ text name ]
            , h2 [ class "subtitle is-6" ] [ text (displayFileSize size) ]
            , progressBar progress
            ]
        ]


imageUploadComplete id name size previewUrl =
    div [ class "image-upload image-upload--success" ]
        [ mediaObject
            [ previewImage previewUrl
            ]
            [ h1 [ class "title is-5" ] [ text name ]
            , h2 [ class "subtitle is-6" ] [ text (displayFileSize size) ]
            , h2 [ class "has-text-success subtitle is-6" ] [ text "Complete!" ]
            ]
        ]


imageUploadFailed id name size previewUrl =
    div [ class "image-upload image-upload--failed" ]
        [ mediaObject
            [ previewImage previewUrl
            ]
            [ h1 [ class "title is-5" ] [ text name ]
            , h2 [ class "subtitle is-6" ] [ text (displayFileSize size) ]
            , h2 [ class "has-text-danger subtitle is-6" ] [ text "Failed!" ]
            ]
        ]


imageUploads fileUploads =
    div [ class "image-uploads" ]
        (List.map
            (\fileUpload ->
                let
                    name =
                        File.name fileUpload.file

                    size =
                        File.size fileUpload.file
                in
                case fileUpload.progress of
                    Waiting ->
                        imageUploadWaiting fileUpload.id name size fileUpload.previewUrl

                    InProgress progress ->
                        imageUploadInProgress fileUpload.id name size fileUpload.previewUrl progress

                    Complete ->
                        imageUploadComplete fileUpload.id name size fileUpload.previewUrl

                    _ ->
                        imageUploadFailed fileUpload.id name size fileUpload.previewUrl
            )
            fileUploads
        )


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


uploadArea =
    div
        [ class "upload-area"
        , hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ h2 [ class "subtitle is-3" ]
            [ text "Drop images here"
            ]
        ]


view model =
    div []
        [ uploadArea
        , imageUploads model.imageUploads
        ]
