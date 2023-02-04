-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Dphones.Enum.Album_select_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "album"

  - Date - column name
  - Tag - column name
  - Title - column name

-}
type Album_select_column
    = Date
    | Tag
    | Title


list : List Album_select_column
list =
    [ Date, Tag, Title ]


decoder : Decoder Album_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "date" ->
                        Decode.succeed Date

                    "tag" ->
                        Decode.succeed Tag

                    "title" ->
                        Decode.succeed Title

                    _ ->
                        Decode.fail ("Invalid Album_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Album_select_column -> String
toString enum____ =
    case enum____ of
        Date ->
            "date"

        Tag ->
            "tag"

        Title ->
            "title"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Album_select_column
fromString enumString____ =
    case enumString____ of
        "date" ->
            Just Date

        "tag" ->
            Just Tag

        "title" ->
            Just Title

        _ ->
            Nothing
