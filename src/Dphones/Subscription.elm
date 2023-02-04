-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Dphones.Subscription exposing (..)

import Dphones.Enum.Album_select_column
import Dphones.Enum.Mixen_select_column
import Dphones.Enum.Set_select_column
import Dphones.Enum.Track_select_column
import Dphones.InputObject
import Dphones.Interface
import Dphones.Object
import Dphones.Scalar
import Dphones.ScalarCodecs
import Dphones.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)


type alias AlbumOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Album_select_column.Album_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Album_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Album_bool_exp
    }


{-| fetch data from the table: "album"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
album :
    (AlbumOptionalArguments -> AlbumOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Album
    -> SelectionSet (List decodesTo) RootSubscription
album fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Album_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeAlbum_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeAlbum_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "album" optionalArgs____ object____ (Basics.identity >> Decode.list)


type alias AlbumAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Album_select_column.Album_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Album_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Album_bool_exp
    }


{-| fetch aggregated fields from the table: "album"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
album_aggregate :
    (AlbumAggregateOptionalArguments -> AlbumAggregateOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Album_aggregate
    -> SelectionSet decodesTo RootSubscription
album_aggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Album_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeAlbum_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeAlbum_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "album_aggregate" optionalArgs____ object____ Basics.identity


type alias AlbumByPkRequiredArguments =
    { tag : String }


{-| fetch data from the table: "album" using primary key columns
-}
album_by_pk :
    AlbumByPkRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Album
    -> SelectionSet (Maybe decodesTo) RootSubscription
album_by_pk requiredArgs____ object____ =
    Object.selectionForCompositeField "album_by_pk" [ Argument.required "tag" requiredArgs____.tag Encode.string ] object____ (Basics.identity >> Decode.nullable)


type alias AlbumStreamOptionalArguments =
    { where_ : OptionalArgument Dphones.InputObject.Album_bool_exp }


type alias AlbumStreamRequiredArguments =
    { batch_size : Int
    , cursor : List (Maybe Dphones.InputObject.Album_stream_cursor_input)
    }


{-| fetch data from the table in a streaming manner: "album"

  - batch\_size - maximum number of rows returned in a single batch
  - cursor - cursor to stream the results returned by the query
  - where\_ - filter the rows returned

-}
album_stream :
    (AlbumStreamOptionalArguments -> AlbumStreamOptionalArguments)
    -> AlbumStreamRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Album
    -> SelectionSet (List decodesTo) RootSubscription
album_stream fillInOptionals____ requiredArgs____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeAlbum_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "album_stream" (optionalArgs____ ++ [ Argument.required "batch_size" requiredArgs____.batch_size Encode.int, Argument.required "cursor" requiredArgs____.cursor (Dphones.InputObject.encodeAlbum_stream_cursor_input |> Encode.maybe |> Encode.list) ]) object____ (Basics.identity >> Decode.list)


type alias MixenOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Mixen_select_column.Mixen_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Mixen_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Mixen_bool_exp
    }


{-| An array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
mixen :
    (MixenOptionalArguments -> MixenOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Mixen
    -> SelectionSet (List decodesTo) RootSubscription
mixen fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Mixen_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeMixen_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeMixen_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "mixen" optionalArgs____ object____ (Basics.identity >> Decode.list)


type alias MixenAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Mixen_select_column.Mixen_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Mixen_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Mixen_bool_exp
    }


{-| An aggregate relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
mixen_aggregate :
    (MixenAggregateOptionalArguments -> MixenAggregateOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Mixen_aggregate
    -> SelectionSet decodesTo RootSubscription
mixen_aggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Mixen_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeMixen_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeMixen_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "mixen_aggregate" optionalArgs____ object____ Basics.identity


type alias MixenByPkRequiredArguments =
    { url : String }


{-| fetch data from the table: "mixen" using primary key columns
-}
mixen_by_pk :
    MixenByPkRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Mixen
    -> SelectionSet (Maybe decodesTo) RootSubscription
mixen_by_pk requiredArgs____ object____ =
    Object.selectionForCompositeField "mixen_by_pk" [ Argument.required "url" requiredArgs____.url Encode.string ] object____ (Basics.identity >> Decode.nullable)


type alias MixenStreamOptionalArguments =
    { where_ : OptionalArgument Dphones.InputObject.Mixen_bool_exp }


type alias MixenStreamRequiredArguments =
    { batch_size : Int
    , cursor : List (Maybe Dphones.InputObject.Mixen_stream_cursor_input)
    }


{-| fetch data from the table in a streaming manner: "mixen"

  - batch\_size - maximum number of rows returned in a single batch
  - cursor - cursor to stream the results returned by the query
  - where\_ - filter the rows returned

-}
mixen_stream :
    (MixenStreamOptionalArguments -> MixenStreamOptionalArguments)
    -> MixenStreamRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Mixen
    -> SelectionSet (List decodesTo) RootSubscription
mixen_stream fillInOptionals____ requiredArgs____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeMixen_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "mixen_stream" (optionalArgs____ ++ [ Argument.required "batch_size" requiredArgs____.batch_size Encode.int, Argument.required "cursor" requiredArgs____.cursor (Dphones.InputObject.encodeMixen_stream_cursor_input |> Encode.maybe |> Encode.list) ]) object____ (Basics.identity >> Decode.list)


type alias SetOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Set_select_column.Set_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Set_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Set_bool_exp
    }


{-| fetch data from the table: "set"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
set :
    (SetOptionalArguments -> SetOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Set
    -> SelectionSet (List decodesTo) RootSubscription
set fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Set_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeSet_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeSet_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "set" optionalArgs____ object____ (Basics.identity >> Decode.list)


type alias SetAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Set_select_column.Set_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Set_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Set_bool_exp
    }


{-| fetch aggregated fields from the table: "set"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
set_aggregate :
    (SetAggregateOptionalArguments -> SetAggregateOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Set_aggregate
    -> SelectionSet decodesTo RootSubscription
set_aggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Set_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeSet_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeSet_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "set_aggregate" optionalArgs____ object____ Basics.identity


type alias SetByPkRequiredArguments =
    { tag : String }


{-| fetch data from the table: "set" using primary key columns
-}
set_by_pk :
    SetByPkRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Set
    -> SelectionSet (Maybe decodesTo) RootSubscription
set_by_pk requiredArgs____ object____ =
    Object.selectionForCompositeField "set_by_pk" [ Argument.required "tag" requiredArgs____.tag Encode.string ] object____ (Basics.identity >> Decode.nullable)


type alias SetStreamOptionalArguments =
    { where_ : OptionalArgument Dphones.InputObject.Set_bool_exp }


type alias SetStreamRequiredArguments =
    { batch_size : Int
    , cursor : List (Maybe Dphones.InputObject.Set_stream_cursor_input)
    }


{-| fetch data from the table in a streaming manner: "set"

  - batch\_size - maximum number of rows returned in a single batch
  - cursor - cursor to stream the results returned by the query
  - where\_ - filter the rows returned

-}
set_stream :
    (SetStreamOptionalArguments -> SetStreamOptionalArguments)
    -> SetStreamRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Set
    -> SelectionSet (List decodesTo) RootSubscription
set_stream fillInOptionals____ requiredArgs____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeSet_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "set_stream" (optionalArgs____ ++ [ Argument.required "batch_size" requiredArgs____.batch_size Encode.int, Argument.required "cursor" requiredArgs____.cursor (Dphones.InputObject.encodeSet_stream_cursor_input |> Encode.maybe |> Encode.list) ]) object____ (Basics.identity >> Decode.list)


type alias TrackOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Track_select_column.Track_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Track_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Track_bool_exp
    }


{-| fetch data from the table: "track"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
track :
    (TrackOptionalArguments -> TrackOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Track
    -> SelectionSet (List decodesTo) RootSubscription
track fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Track_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeTrack_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeTrack_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "track" optionalArgs____ object____ (Basics.identity >> Decode.list)


type alias TrackAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Dphones.Enum.Track_select_column.Track_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Dphones.InputObject.Track_order_by)
    , where_ : OptionalArgument Dphones.InputObject.Track_bool_exp
    }


{-| fetch aggregated fields from the table: "track"

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
track_aggregate :
    (TrackAggregateOptionalArguments -> TrackAggregateOptionalArguments)
    -> SelectionSet decodesTo Dphones.Object.Track_aggregate
    -> SelectionSet decodesTo RootSubscription
track_aggregate fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "distinct_on" filledInOptionals____.distinct_on (Encode.enum Dphones.Enum.Track_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "offset" filledInOptionals____.offset Encode.int, Argument.optional "order_by" filledInOptionals____.order_by (Dphones.InputObject.encodeTrack_order_by |> Encode.list), Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeTrack_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "track_aggregate" optionalArgs____ object____ Basics.identity


type alias TrackByPkRequiredArguments =
    { url : String }


{-| fetch data from the table: "track" using primary key columns
-}
track_by_pk :
    TrackByPkRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Track
    -> SelectionSet (Maybe decodesTo) RootSubscription
track_by_pk requiredArgs____ object____ =
    Object.selectionForCompositeField "track_by_pk" [ Argument.required "url" requiredArgs____.url Encode.string ] object____ (Basics.identity >> Decode.nullable)


type alias TrackStreamOptionalArguments =
    { where_ : OptionalArgument Dphones.InputObject.Track_bool_exp }


type alias TrackStreamRequiredArguments =
    { batch_size : Int
    , cursor : List (Maybe Dphones.InputObject.Track_stream_cursor_input)
    }


{-| fetch data from the table in a streaming manner: "track"

  - batch\_size - maximum number of rows returned in a single batch
  - cursor - cursor to stream the results returned by the query
  - where\_ - filter the rows returned

-}
track_stream :
    (TrackStreamOptionalArguments -> TrackStreamOptionalArguments)
    -> TrackStreamRequiredArguments
    -> SelectionSet decodesTo Dphones.Object.Track
    -> SelectionSet (List decodesTo) RootSubscription
track_stream fillInOptionals____ requiredArgs____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { where_ = Absent }

        optionalArgs____ =
            [ Argument.optional "where" filledInOptionals____.where_ Dphones.InputObject.encodeTrack_bool_exp ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "track_stream" (optionalArgs____ ++ [ Argument.required "batch_size" requiredArgs____.batch_size Encode.int, Argument.required "cursor" requiredArgs____.cursor (Dphones.InputObject.encodeTrack_stream_cursor_input |> Encode.maybe |> Encode.list) ]) object____ (Basics.identity >> Decode.list)
