module Route.Album.Album_ exposing (ActionData, Data, Model, Msg, route)

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
import Shared
import View exposing (View)


type alias Data =
    AlbumRenderer.Volume


type alias Model =
    Data


type Msg
    = NoOp


type alias RouteParams =
    { album : String }


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
                \_ _ staticPayload ->
                    ( staticPayload.data, Effect.loaded staticPayload.data )
            , update =
                \_ sharedModel static msg model ->
                    ( model, Effect.none )
            , subscriptions =
                \maybePageUrl routeParams path sharedModel model ->
                    Sub.none
            }


data : RouteParams -> Request.Parser (BackendTask FatalError (Response Data ErrorPage))
data routeParams =
    let
        traxToBody : String -> String -> List AlbumRenderer.Track -> Data
        traxToBody name tag trax =
            { name = name, tag = tag, trax = trax }

        getAlbumWhere : Dphones.InputObject.Album_bool_expOptionalFields -> Dphones.InputObject.Album_bool_expOptionalFields
        getAlbumWhere optionals =
            { optionals
                | tag =
                    Dphones.InputObject.buildString_comparison_exp
                        (\compareOptionals ->
                            { compareOptionals | eq_ = Present routeParams.album }
                        )
                        |> Present
            }

        albumParams : Dphones.Query.AlbumOptionalArguments -> Dphones.Query.AlbumOptionalArguments
        albumParams args =
            { args
                | where_ = Present (Dphones.InputObject.buildAlbum_bool_exp getAlbumWhere)
            }

        getTrackOrder : Dphones.InputObject.Track_order_byOptionalFields -> Dphones.InputObject.Track_order_byOptionalFields
        getTrackOrder args =
            { args | index = Present Dphones.Enum.Order_by.Asc }

        getTrackWhere : Dphones.InputObject.Track_bool_expOptionalFields -> Dphones.InputObject.Track_bool_expOptionalFields
        getTrackWhere optionals =
            { optionals
                | album =
                    Dphones.InputObject.buildString_comparison_exp
                        (\compareOptionals ->
                            { compareOptionals
                                | eq_ = Present routeParams.album
                            }
                        )
                        |> Present
            }

        trackParams : Dphones.Query.TrackOptionalArguments -> Dphones.Query.TrackOptionalArguments
        trackParams args =
            { args
                | order_by = Present [ Dphones.InputObject.buildTrack_order_by getTrackOrder ]
                , where_ = Present (Dphones.InputObject.buildTrack_bool_exp getTrackWhere)
            }
    in
    Dphones.Query.album albumParams AlbumRenderer.albumSelection
        |> Hasura.backendTask
        |> BackendTask.andThen
            (\albums ->
                let
                    name =
                        albums |> List.head |> Maybe.map .title |> Maybe.withDefault ""

                    tag =
                        albums |> List.head |> Maybe.map .tag |> Maybe.withDefault ""
                in
                Dphones.Query.track trackParams AlbumRenderer.trackSelection
                    |> Hasura.backendTask
                    |> BackendTask.map (traxToBody name tag >> Response.render)
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
