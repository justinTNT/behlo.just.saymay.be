module Route.Albums exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Dphones.Enum.Order_by
import Dphones.InputObject
import Dphones.Object
import Dphones.Object.Album
import Dphones.Object.Track
import Dphones.Query
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Hasura
import Head
import Head.Seo as Seo
import Html.Styled as Html
import Html.Styled.Attributes exposing (attribute, class, href, src)
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Regex
import RouteBuilder exposing (StatefulRoute, StaticPayload)
import Server.Request as Request
import Server.Response as Response exposing (Response)
import Shared
import View exposing (View)


type alias Data =
    { albums : List Album
    }


type alias Msg =
    ()


type alias ActionData =
    {}


type alias Model =
    {}


type alias RouteParams =
    {}


route : StatefulRoute RouteParams Data actionData {} ()
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = \_ -> Request.skip "No action."
        }
        |> RouteBuilder.buildNoState { view = view }


type alias Track =
    { index : Maybe Int, url : String }


type alias Album =
    { tag : String, title : String, lists : List Track }


traxSelection : SelectionSet Track Dphones.Object.Track
traxSelection =
    SelectionSet.map2 Track
        Dphones.Object.Track.index
        Dphones.Object.Track.url


albumSelection : SelectionSet Album Dphones.Object.Album
albumSelection =
    let
        getWhere : Dphones.InputObject.Track_bool_expOptionalFields -> Dphones.InputObject.Track_bool_expOptionalFields
        getWhere optionals =
            { optionals
                | index =
                    Dphones.InputObject.buildInt_comparison_exp
                        (\compareOptionals ->
                            { compareOptionals | eq_ = Present 1 }
                        )
                        |> Present
            }

        params args =
            { args | where_ = Present (Dphones.InputObject.buildTrack_bool_exp getWhere) }
    in
    SelectionSet.map3 Album
        Dphones.Object.Album.tag
        Dphones.Object.Album.title
        (Dphones.Object.Album.lists params traxSelection)


data : RouteParams -> Request.Parser (BackendTask FatalError (Response Data ErrorPage))
data _ =
    let
        albumsToBody : List Album -> Data
        albumsToBody albums =
            { albums = albums }

        getAlbumOrder : Dphones.InputObject.Album_order_byOptionalFields -> Dphones.InputObject.Album_order_byOptionalFields
        getAlbumOrder args =
            { args | date = Present Dphones.Enum.Order_by.Desc }

        params : Dphones.Query.AlbumOptionalArguments -> Dphones.Query.AlbumOptionalArguments
        params args =
            { args
                | order_by = Present [ Dphones.InputObject.buildAlbum_order_by getAlbumOrder ]
            }
    in
    Dphones.Query.album params albumSelection
        |> Hasura.backendTask
        |> BackendTask.map (albumsToBody >> Response.render)
        |> Request.succeed


head :
    StaticPayload Data actionData RouteParams
    -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "just.saymay.be"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "Behlo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "underground aussie electronicaNT"
        , locale = Nothing
        , title = "Behlo - electronicaNT"
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data actionData RouteParams
    -> View (Pages.Msg.Msg ())
view _ _ static =
    let
        myReplace : String -> (Regex.Match -> String) -> String -> String
        myReplace userRegex replacer string =
            case Regex.fromString userRegex of
                Nothing ->
                    string

                Just regex ->
                    Regex.replace regex replacer string

        albumToHtml album =
            Html.a [ href ("/album/" ++ album.tag) ]
                [ Html.div [ class "listed-album-cover" ]
                    [ Html.img [ src (List.head album.lists |> Maybe.map (.url >> myReplace "[^/]*.mp3" (\_ -> album.tag ++ ".jpg")) |> Maybe.withDefault "") ] []
                    , Html.span [] [ Html.text album.title ]
                    ]
                ]
    in
    { title = "DJ Dope Inc."
    , body =
        [ Html.div [ attribute "style" "padding-left: 2em" ]
            (Html.h2 [ attribute "style" "margin-left: 2em" ] [ Html.text "Behlo" ]
                :: List.map albumToHtml static.data.albums
            )
        ]
    }
