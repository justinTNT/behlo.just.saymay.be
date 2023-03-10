module AlbumRenderer exposing (Album, Track, Volume, albumSelection, trackSelection, view)

import Dphones.Object
import Dphones.Object.Album
import Dphones.Object.Track
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (attribute, class, href, id, src, type_)
import Regex


type alias Album =
    { tag : String, title : String }


type alias Track =
    { title : String, url : String }


type alias Volume =
    { name : String, tag : String, trax : List Track }


albumSelection : SelectionSet Album Dphones.Object.Album
albumSelection =
    SelectionSet.map2 Album
        Dphones.Object.Album.tag
        Dphones.Object.Album.title


trackSelection : SelectionSet Track Dphones.Object.Track
trackSelection =
    SelectionSet.map2 Track
        Dphones.Object.Track.title
        Dphones.Object.Track.url


view : Volume -> Html msg
view volume =
    let
        classNameStr =
            case volume.trax of
                _ :: [] ->
                    "single"

                _ ->
                    "multi"

        myReplace : String -> (Regex.Match -> String) -> String -> String
        myReplace userRegex replacer string =
            case Regex.fromString userRegex of
                Nothing ->
                    string

                Just regex ->
                    Regex.replace regex replacer string

        trackToHtml zIndex track =
            let
                url =
                    myReplace "[^/]*.mp3" (\_ -> volume.tag ++ ".jpg") track.url
            in
            Html.div
                [ class "song"
                , class "amplitude-song-container"
                , class "amplitude-play-pause"
                , attribute "data-amplitude-song-index" (String.fromInt zIndex)
                , attribute "background-image" ("url(\"" ++ url ++ "\")")
                ]
                [ Html.div [ class "song-now-playing-icon-container" ]
                    [ Html.div [ class "play-button-container" ] []
                    , Html.img [ class "now-playing", src "/images/Now Playing.svg" ] []
                    ]
                , Html.div [ class "song-meta-data" ]
                    [ Html.span [ class "song-title" ] [ Html.text track.title ]
                    , Html.span [ class "song-artist" ] [ Html.text "Behlo" ]
                    ]
                ]
    in
    Html.div [ class classNameStr ]
        [ Html.div [ id "amplitude-player" ]
            [ Html.div [ id "amplitude-left" ]
                [ Html.img [ attribute "data-amplitude-song-info" "cover_art_url", class "album-art", class "cover" ] []
                , Html.img [ attribute "data-amplitude-song-info" "cover_art_url", class "album-art", class "disc" ] []
                , Html.div [ class "amplitude-visualization", id "large-visualization" ] []
                , Html.div [ id "player-left-bottom" ]
                    [ Html.div [ id "time-container" ]
                        [ Html.span [ class "current-time" ]
                            [ Html.span [ class "amplitude-current-minutes" ] []
                            , Html.text ":"
                            , Html.span [ class "amplitude-current-seconds" ] []
                            ]
                        ]
                    , Html.div [ id "progress-container" ]
                        [ Html.div [ class "amplitude-wave-form" ] []
                        , Html.input [ type_ "range", class "amplitude-song-slider" ] []
                        , Html.progress [ id "song-played-progress", class "amplitude-song-played-progress" ] []
                        , Html.progress [ id "song-buffered-progress", class "amplitude-song-buffered-progress" ] []
                        ]
                    , Html.span [ class "duration" ]
                        []
                    ]
                , Html.div [ id "control-container" ]
                    [ Html.div [ id "repeat-container" ]
                        [ Html.div [ id "repeat", class "amplitude-repeat" ] []
                        , Html.div [ id "shuffle", class "amplitude-shuffle", class "amplitude-shuffle-off" ] []
                        ]
                    , Html.div [ id "central-control-container" ]
                        [ Html.div [ id "central-controls" ]
                            [ Html.div [ id "previous", class "amplitude-prev" ] []
                            , Html.div [ id "play-pause", class "amplitude-play-pause" ] []
                            , Html.div [ id "next", class "amplitude-next" ] []
                            ]
                        ]
                    , Html.div [ id "volume-container" ]
                        [ Html.div [ id "volume-controls" ]
                            [ Html.div [ class "amplitude-mute", class "amplitude-not-muted" ] []
                            , Html.input [ type_ "range", class "amplitude-volume-slider" ] []
                            , Html.div [ class "ms-range-fix" ] []
                            ]
                        , Html.div [ id "shuffle-right", class "amplitude-shuffle", class "amplitude-shuffle-off" ] []
                        ]
                    ]
                , Html.div [ id "meta-container" ]
                    [ Html.span [ class "song-name", attribute "data-amplitude-song-info" "name" ] []
                    , Html.div [ class "song-artist-album" ]
                        [ Html.span [ attribute "data-amplitude-song-info" "artist" ] []
                        , Html.span [ attribute "data-amplitude-song-info" "album" ] []
                        ]
                    ]
                , Html.div [ class "more" ]
                    [ Html.a [ href "/albums" ] [ Html.text "more ..." ]
                    ]
                ]
            , Html.div [ id "amplitude-right" ]
                (List.concat
                    [ [ Html.h2 [ attribute "style" "margin-left: 2em" ] [ Html.text volume.name ] ]
                    , List.indexedMap trackToHtml volume.trax
                    , [ Html.a [ href "/albums", attribute "style" "padding: 1em 11em" ] [ Html.text "more ..." ] ]
                    ]
                )
            ]
        ]
