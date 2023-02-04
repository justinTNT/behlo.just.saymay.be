-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Dphones.Object.Track_aggregate_fields exposing (..)

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
import Json.Decode as Decode


avg :
    SelectionSet decodesTo Dphones.Object.Track_avg_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
avg object____ =
    Object.selectionForCompositeField "avg" [] object____ (Basics.identity >> Decode.nullable)


type alias CountOptionalArguments =
    { columns : OptionalArgument (List Dphones.Enum.Track_select_column.Track_select_column)
    , distinct : OptionalArgument Bool
    }


count :
    (CountOptionalArguments -> CountOptionalArguments)
    -> SelectionSet Int Dphones.Object.Track_aggregate_fields
count fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { columns = Absent, distinct = Absent }

        optionalArgs____ =
            [ Argument.optional "columns" filledInOptionals____.columns (Encode.enum Dphones.Enum.Track_select_column.toString |> Encode.list), Argument.optional "distinct" filledInOptionals____.distinct Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "Int" "count" optionalArgs____ Decode.int


max :
    SelectionSet decodesTo Dphones.Object.Track_max_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
max object____ =
    Object.selectionForCompositeField "max" [] object____ (Basics.identity >> Decode.nullable)


min :
    SelectionSet decodesTo Dphones.Object.Track_min_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
min object____ =
    Object.selectionForCompositeField "min" [] object____ (Basics.identity >> Decode.nullable)


stddev :
    SelectionSet decodesTo Dphones.Object.Track_stddev_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
stddev object____ =
    Object.selectionForCompositeField "stddev" [] object____ (Basics.identity >> Decode.nullable)


stddev_pop :
    SelectionSet decodesTo Dphones.Object.Track_stddev_pop_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
stddev_pop object____ =
    Object.selectionForCompositeField "stddev_pop" [] object____ (Basics.identity >> Decode.nullable)


stddev_samp :
    SelectionSet decodesTo Dphones.Object.Track_stddev_samp_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
stddev_samp object____ =
    Object.selectionForCompositeField "stddev_samp" [] object____ (Basics.identity >> Decode.nullable)


sum :
    SelectionSet decodesTo Dphones.Object.Track_sum_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
sum object____ =
    Object.selectionForCompositeField "sum" [] object____ (Basics.identity >> Decode.nullable)


var_pop :
    SelectionSet decodesTo Dphones.Object.Track_var_pop_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
var_pop object____ =
    Object.selectionForCompositeField "var_pop" [] object____ (Basics.identity >> Decode.nullable)


var_samp :
    SelectionSet decodesTo Dphones.Object.Track_var_samp_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
var_samp object____ =
    Object.selectionForCompositeField "var_samp" [] object____ (Basics.identity >> Decode.nullable)


variance :
    SelectionSet decodesTo Dphones.Object.Track_variance_fields
    -> SelectionSet (Maybe decodesTo) Dphones.Object.Track_aggregate_fields
variance object____ =
    Object.selectionForCompositeField "variance" [] object____ (Basics.identity >> Decode.nullable)
