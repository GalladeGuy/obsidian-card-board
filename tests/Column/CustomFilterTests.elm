module Column.CustomFilterTests exposing (suite)

import Date exposing (Date)
import Column.CustomFilter as CustomFilterColumn exposing (CustomFilterColumn)
import Expect
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import PlacementResult
import TaskItem exposing (TaskItem)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ addTaskItem
        , dateTokens
        , decoder
        , encoder
        , init
        , setCollapse
        , setTagsToHide
        , tag
        , toList
        , toggleCollapse
        , updateName
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ describe "Filter expression is just a tag"
            [ test "Places an incomplete task item with no sub-tasks and a matching tag" <|
                \() ->
                    CustomFilterColumn.init "" "#atag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo #atag")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "Places an incomplete task item with no sub-tasks and a matching tag (amongst others)" <|
                \() ->
                    CustomFilterColumn.init "" "#btag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo #atag #btag #ctag")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "Places an incomplete task item with no tags and an incomplete sub-task with a matching tag" <|
                \() ->
                    CustomFilterColumn.init "" "#atag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo\n  - [ ] bar #atag")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "Places an incomplete task item with no tags and an incomplete sub-task with a matching tag (amongst others)" <|
                \() ->
                    CustomFilterColumn.init "" "#btag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo\n  - [ ] bar #atag #btag #ctag")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with no tags and no sub-tasks" <|
                \() ->
                    CustomFilterColumn.init "" "#atag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "DoesNotBelong an incomplete task item with a non-matching tag and no sub-tasks" <|
                \() ->
                    CustomFilterColumn.init "" "#atag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo #xtag")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "DoesNotBelong an incomplete task item with no tags and an incomplete sub-task with a non-matching tag" <|
                \() ->
                    CustomFilterColumn.init "" "#atag"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo\n  - [ ] bar #xtag")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "Filter expression is a date expression"
            [ test "Places an incomplete task item with due date matching an \"on date\" expression" <|
                \() ->
                    CustomFilterColumn.init "" "on 2024-01-01"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo @due(2024-01-01)")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with due date not matching an \"on date\" expression" <|
                \() ->
                    CustomFilterColumn.init "" "on 2024-01-01"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo @due(2024-01-02)")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ,test "Places an incomplete task item with due date matching a \"before date\" expression" <|
                \() ->
                    CustomFilterColumn.init "" "before 2024-01-03"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo @due(2024-01-01)")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with due date not matching a \"before date\" expression" <|
                \() ->
                    CustomFilterColumn.init "" "before 2024-01-03"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo @due(2024-01-03)")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "Places an incomplete task item with due date matching an \"after date\" expression" <|
                \() ->
                    CustomFilterColumn.init "" "after 2024-01-01"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo @due(2024-01-03)")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with due date not matching an \"after date\" expression" <|
                \() ->
                    CustomFilterColumn.init "" "after 2024-01-01"
                        |> CustomFilterColumn.addTaskItem today (taskItem "- [ ] foo @due(2024-01-01)")
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decodes filterExpression field" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.filterExpression
                    |> Expect.equal (Ok "#aTag and 2024-01-01")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.tagsToHide
                    |> Expect.equal (Ok [])
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a decoded column" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample CustomFilterColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                CustomFilterColumn.init "" ""
                    |> CustomFilterColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                CustomFilterColumn.init "" ""
                    |> CustomFilterColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                CustomFilterColumn.init "A Column Name" ""
                    |> CustomFilterColumn.name
                    |> Expect.equal "A Column Name"
        , test "sets the column filter expression" <|
            \() ->
                CustomFilterColumn.init "" "#aTag and 2024-01-01"
                    |> CustomFilterColumn.filterExpression
                    |> Expect.equal "#aTag and 2024-01-01"
        , test "is not collapsed" <|
            \() ->
                CustomFilterColumn.init "" ""
                    |> CustomFilterColumn.isCollapsed
                    |> Expect.equal False
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map (CustomFilterColumn.setCollapse True)
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map (CustomFilterColumn.setCollapse True)
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map (CustomFilterColumn.setCollapse False)
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map (CustomFilterColumn.setCollapse False)
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                CustomFilterColumn.init "" ""
                    |> CustomFilterColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> CustomFilterColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


tag : Test
tag =
    describe "tag"
        [ test "returns the column's filter expression" <|
            \() ->
                CustomFilterColumn.init "" "#aTag and 2024-01-01"
                    |> CustomFilterColumn.filterExpression
                    |> Expect.equal "#aTag and 2024-01-01"
        ]


today : Date
today =
    DateTimeHelpers.todayDate


dateTokens : Test
dateTokens =
    describe "Date Tokens"
        [ describe "today token"
            [ test "Places an incomplete task item with due date matching 'on today'" <|
                \() ->
                    CustomFilterColumn.init "" "on today"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.today ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with due date not matching 'on today'" <|
                \() ->
                    CustomFilterColumn.init "" "on today"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.tomorrow ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "yesterday token"
            [ test "Places an incomplete task item with due date matching 'on yesterday'" <|
                \() ->
                    CustomFilterColumn.init "" "on yesterday"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.yesterday ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with due date not matching 'on yesterday'" <|
                \() ->
                    CustomFilterColumn.init "" "on yesterday"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.today ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "tomorrow token"
            [ test "Places an incomplete task item with due date matching 'on tomorrow'" <|
                \() ->
                    CustomFilterColumn.init "" "on tomorrow"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.tomorrow ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with due date not matching 'on tomorrow'" <|
                \() ->
                    CustomFilterColumn.init "" "on tomorrow"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.today ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "before/after with date tokens"
            [ test "Places an incomplete task item with due date matching 'before tomorrow'" <|
                \() ->
                    CustomFilterColumn.init "" "before tomorrow"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.today ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "Places an incomplete task item with due date matching 'after yesterday'" <|
                \() ->
                    CustomFilterColumn.init "" "after yesterday"
                        |> CustomFilterColumn.addTaskItem today (taskItem ("- [ ] foo @due(" ++ DateTimeHelpers.today ++ ")"))
                        |> Tuple.mapFirst CustomFilterColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            ]
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by due date and title (not case sensitive)" <|
            \() ->
                CustomFilterColumn.init "" "#atag"
                    |> justAdd (taskItem "- [ ] f #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a #atag @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B #atag @due(2022-01-03)")
                    |> CustomFilterColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.toggleCollapse
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","expression":"#aTag and 2024-01-01"}"""
                    |> DecodeHelpers.runDecoder CustomFilterColumn.decoder
                    |> .decoded
                    |> Result.map CustomFilterColumn.toggleCollapse
                    |> Result.map CustomFilterColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name" <|
            \() ->
                CustomFilterColumn.init "A Column Name" "#aTag and 2024-01-01"
                    |> CustomFilterColumn.updateName "new name"
                    |> CustomFilterColumn.name
                    |> Expect.equal "new name"
        ]



-- HELPERS


justAdd : TaskItem -> CustomFilterColumn -> CustomFilterColumn
justAdd item column =
    column
        |> CustomFilterColumn.addTaskItem today item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
