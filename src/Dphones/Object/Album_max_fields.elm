-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Dphones.Object.Album_max_fields exposing (..)

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


date : SelectionSet (Maybe Dphones.ScalarCodecs.Date) Dphones.Object.Album_max_fields
date =
    Object.selectionForField "(Maybe ScalarCodecs.Date)" "date" [] (Dphones.ScalarCodecs.codecs |> Dphones.Scalar.unwrapCodecs |> .codecDate |> .decoder |> Decode.nullable)


tag : SelectionSet (Maybe String) Dphones.Object.Album_max_fields
tag =
    Object.selectionForField "(Maybe String)" "tag" [] (Decode.string |> Decode.nullable)


title : SelectionSet (Maybe String) Dphones.Object.Album_max_fields
title =
    Object.selectionForField "(Maybe String)" "title" [] (Decode.string |> Decode.nullable)
