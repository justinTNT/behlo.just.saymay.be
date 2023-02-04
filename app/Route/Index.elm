module Route.Index exposing (ActionData, Data, Model, Msg, route)

import AlbumRenderer
import BackendTask exposing (BackendTask)
import Dphones.Enum.Order_by
import Dphones.InputObject
import Dphones.Query
import Effect
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Hasura
import Head
import Head.Seo as Seo
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import RouteBuilder exposing (StatefulRoute, StaticPayload)
import Server.Request as Request
import Server.Response as Response exposing (Response)
import Shared exposing (..)
import View exposing (View)


type alias Data =
    AlbumRenderer.Volume


type alias Model =
    Data


type Msg
    = NoOp


type alias RouteParams =
    {}


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = \_ -> Request.skip "No action."
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init =
                \maybePageUrl sharedModel staticPayload ->
                    ( staticPayload.data, Effect.loaded staticPayload.data )
            , update =
                \pageUrl sharedModel static msg model ->
                    ( model, Effect.none )
            , subscriptions =
                \maybePageUrl routeParams path sharedModel model ->
                    Sub.none
            }


data : RouteParams -> Request.Parser (BackendTask FatalError (Response Data ErrorPage))
data routeParams =
    let
        trackToBody : Maybe AlbumRenderer.Album -> List AlbumRenderer.Track -> Data
        trackToBody albumO trax =
            case albumO of
                Nothing ->
                    { name = "Nothing to see, hear", tag = "", trax = [] }

                Just album ->
                    { name = album.title, tag = album.tag, trax = trax }

        getAlbumOrder : Dphones.InputObject.Album_order_byOptionalFields -> Dphones.InputObject.Album_order_byOptionalFields
        getAlbumOrder args =
            { args | date = Present Dphones.Enum.Order_by.Desc }

        params : Dphones.Query.AlbumOptionalArguments -> Dphones.Query.AlbumOptionalArguments
        params args =
            { args
                | order_by = Present [ Dphones.InputObject.buildAlbum_order_by getAlbumOrder ]
            }

        getTrackOrder : Dphones.InputObject.Track_order_byOptionalFields -> Dphones.InputObject.Track_order_byOptionalFields
        getTrackOrder args =
            { args | index = Present Dphones.Enum.Order_by.Asc }

        getTrackWhere tag optionals =
            { optionals
                | album =
                    Dphones.InputObject.buildString_comparison_exp
                        (\compareOptionals ->
                            { compareOptionals
                                | eq_ = Present tag
                            }
                        )
                        |> Present
            }

        trackParams : Maybe AlbumRenderer.Album -> Dphones.Query.TrackOptionalArguments -> Dphones.Query.TrackOptionalArguments
        trackParams albumO args =
            case albumO of
                Just album ->
                    { args
                        | order_by = Present [ Dphones.InputObject.buildTrack_order_by getTrackOrder ]
                        , where_ = Present (Dphones.InputObject.buildTrack_bool_exp (getTrackWhere album.tag))
                    }

                Nothing ->
                    args
    in
    Dphones.Query.album params AlbumRenderer.albumSelection
        |> Hasura.backendTask
        |> BackendTask.andThen
            (\albums ->
                let
                    albumO =
                        albums |> List.head
                in
                Dphones.Query.track (trackParams albumO) AlbumRenderer.trackSelection
                    |> Hasura.backendTask
                    |> BackendTask.map (trackToBody albumO >> Response.render)
            )
        |> Request.succeed


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Behlo"
        , image =
            { url = Pages.Url.external "https://s3.ap-southeast-2.amazonaws.com/dphon.es/homebrew/coattails/iraq.jpg"
            , alt = "Behlo logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Behlo - aussie electronicaNT"
        , locale = Nothing
        , title = static.data.name
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg Msg)
view maybeUrl sharedModel model static =
    { title = "Behlo - " ++ static.data.name
    , body = [ AlbumRenderer.view static.data ]
    }
