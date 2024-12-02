module Column exposing
    ( Column(..)
    , addTaskItem
    , asCompletedColumn
    , cardCount
    , cards
    , completed
    , containsTask
    , customFilter
    , dated
    , decoder
    , encoder
    , isCollapsed
    , isCompleted
    , name
    , namedTag
    , namedTagTag
    , otherTags
    , setCollapse
    , setNameToDefault
    , setTagsToHide
    , toggleCollapse
    , typeString
    , undated
    , untagged
    , updateName
    , updateOtherTags
    )

import Card exposing (Card)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.CustomFilter as CustomFilterColumn exposing (CustomFilterColumn)
import Column.Dated as DatedColumn exposing (DatedColumn)
import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Column.Undated as UndatedColumn exposing (UndatedColumn)
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import Date exposing (Date)
import DecodeHelpers
import DefaultColumnNames exposing (DefaultColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import Html exposing (col)



-- TYPES


type Column
    = Completed CompletedColumn
    | CustomFilter CustomFilterColumn
    | Dated DatedColumn
    | NamedTag NamedTagColumn
    | OtherTags OtherTagsColumn
    | Undated UndatedColumn
    | Untagged UntaggedColumn



-- CONSTRUCTION


completed : CompletedColumn -> Column
completed =
    Completed


customFilter : CustomFilterColumn -> Column
customFilter =
    CustomFilter


dated : DatedColumn -> Column
dated =
    Dated


namedTag : String -> String -> Column
namedTag name_ tag_ =
    NamedTag <| NamedTagColumn.init name_ tag_


otherTags : String -> List String -> Column
otherTags name_ ots =
    OtherTags <| OtherTagsColumn.init name_ ots


undated : String -> Column
undated name_ =
    Undated <| UndatedColumn.init name_


untagged : String -> Column
untagged name_ =
    Untagged <| UntaggedColumn.init name_



-- DECODE / ENCODE


decoder : TsDecode.Decoder Column
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "completed" Completed CompletedColumn.decoder
        , DecodeHelpers.toElmVariant "customFilter" CustomFilter CustomFilterColumn.decoder
        , DecodeHelpers.toElmVariant "dated" Dated DatedColumn.decoder
        , DecodeHelpers.toElmVariant "namedTag" NamedTag NamedTagColumn.decoder
        , DecodeHelpers.toElmVariant "otherTags" OtherTags OtherTagsColumn.decoder
        , DecodeHelpers.toElmVariant "undated" Undated UndatedColumn.decoder
        , DecodeHelpers.toElmVariant "untagged" Untagged UntaggedColumn.decoder
        ]


encoder : TsEncode.Encoder Column
encoder =
    TsEncode.union
        (\vCompleted vCustomFilter vDated vNamedTag vOtherTags vUndated vUntagged value ->
            case value of
                Completed config ->
                    vCompleted config

                CustomFilter config ->
                    vCustomFilter config

                Dated config ->
                    vDated config

                NamedTag config ->
                    vNamedTag config

                OtherTags config ->
                    vOtherTags config

                Undated config ->
                    vUndated config

                Untagged config ->
                    vUntagged config
        )
        |> TsEncode.variantTagged "completed" CompletedColumn.encoder
        |> TsEncode.variantTagged "customFilter" CustomFilterColumn.encoder
        |> TsEncode.variantTagged "dated" DatedColumn.encoder
        |> TsEncode.variantTagged "namedTag" NamedTagColumn.encoder
        |> TsEncode.variantTagged "otherTags" OtherTagsColumn.encoder
        |> TsEncode.variantTagged "undated" UndatedColumn.encoder
        |> TsEncode.variantTagged "untagged" UntaggedColumn.encoder
        |> TsEncode.buildUnion



-- INFO


asCompletedColumn : Column -> Maybe CompletedColumn
asCompletedColumn column =
    case column of
        Completed completedColumn ->
            Just completedColumn

        _ ->
            Nothing


cardCount : Column -> Int
cardCount column =
    List.length (toList column)


cards : String -> Column -> List Card
cards boardId column =
    let
        cardIdPrefix : String
        cardIdPrefix =
            boardId ++ ":" ++ name column
    in
    toList column
        |> List.map (Card.fromTaskItem cardIdPrefix (tagsToHide column))


containsTask : String -> Column -> Bool
containsTask taskId column =
    case column of
        Completed completedColumn ->
            CompletedColumn.containsTask taskId completedColumn

        CustomFilter customFilterColumn ->
            CustomFilterColumn.containsTask taskId customFilterColumn

        Dated datedColumn ->
            DatedColumn.containsTask taskId datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.containsTask taskId namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.containsTask taskId otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.containsTask taskId undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.containsTask taskId untaggedColumn


isCollapsed : Column -> Bool
isCollapsed column =
    case column of
        Completed completedColumn ->
            CompletedColumn.isCollapsed completedColumn

        CustomFilter customFilterColumn ->
            CustomFilterColumn.isCollapsed customFilterColumn

        Dated datedColumn ->
            DatedColumn.isCollapsed datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.isCollapsed namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.isCollapsed otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.isCollapsed undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.isCollapsed untaggedColumn


isCompleted : Column -> Bool
isCompleted column =
    case column of
        Completed _ ->
            True

        _ ->
            False


name : Column -> String
name column =
    case column of
        Completed completedColumn ->
            CompletedColumn.name completedColumn

        CustomFilter customFilterColumn ->
            CustomFilterColumn.name customFilterColumn

        Dated datedColumn ->
            DatedColumn.name datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.name namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.name otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.name undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.name untaggedColumn


namedTagTag : Column -> Maybe String
namedTagTag column =
    case column of
        NamedTag namedTagColumn ->
            Just (NamedTagColumn.tag namedTagColumn)

        _ ->
            Nothing


typeString : Column -> String
typeString column =
    case column of
        Completed _ ->
            "Completed"

        CustomFilter _ ->
            "Custom Filter"

        Dated _ ->
            "Dated"

        NamedTag _ ->
            "Tagged"

        OtherTags _ ->
            "Other Tags"

        Undated _ ->
            "Undated"

        Untagged _ ->
            "Untagged"



-- MANIPULATION


addTaskItem : Date -> TaskItem -> Column -> ( Column, PlacementResult )
addTaskItem today taskItem column =
    case column of
        Completed _ ->
            ( column, PlacementResult.DoesNotBelong )

        CustomFilter customFilterColumn ->
            CustomFilterColumn.addTaskItem taskItem customFilterColumn
                |> Tuple.mapFirst CustomFilter

        Dated datedColumn ->
            DatedColumn.addTaskItem today taskItem datedColumn
                |> Tuple.mapFirst Dated

        NamedTag namedTagColumn ->
            NamedTagColumn.addTaskItem taskItem namedTagColumn
                |> Tuple.mapFirst NamedTag

        OtherTags otherTagsColumn ->
            OtherTagsColumn.addTaskItem taskItem otherTagsColumn
                |> Tuple.mapFirst OtherTags

        Undated undatedColumn ->
            UndatedColumn.addTaskItem taskItem undatedColumn
                |> Tuple.mapFirst Undated

        Untagged untaggedColumn ->
            UntaggedColumn.addTaskItem taskItem untaggedColumn
                |> Tuple.mapFirst Untagged


setCollapse : Bool -> Column -> Column
setCollapse newCollapsed column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.setCollapse newCollapsed completedColumn)

        CustomFilter customFilterColumn ->
            CustomFilter (CustomFilterColumn.setCollapse newCollapsed customFilterColumn)

        Dated datedColumn ->
            Dated (DatedColumn.setCollapse newCollapsed datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.setCollapse newCollapsed namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.setCollapse newCollapsed otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.setCollapse newCollapsed undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.setCollapse newCollapsed untaggedColumn)


setNameToDefault : DefaultColumnNames -> Column -> Column
setNameToDefault defaultColumnNames column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.setNameToDefault defaultColumnNames completedColumn)

        CustomFilter _ ->
            column

        Dated datedColumn ->
            Dated (DatedColumn.setNameToDefault defaultColumnNames datedColumn)

        NamedTag _ ->
            column

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.setNameToDefault defaultColumnNames otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.setNameToDefault defaultColumnNames undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.setNameToDefault defaultColumnNames untaggedColumn)


setTagsToHide : List String -> Column -> Column
setTagsToHide tags column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.setTagsToHide tags completedColumn)

        CustomFilter customFilterColumn ->
            CustomFilter (CustomFilterColumn.setTagsToHide tags customFilterColumn)

        Dated datedColumn ->
            Dated (DatedColumn.setTagsToHide tags datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.setTagsToHide tags namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.setTagsToHide tags otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.setTagsToHide tags undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.setTagsToHide tags untaggedColumn)


toggleCollapse : Column -> Column
toggleCollapse column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.toggleCollapse completedColumn)

        CustomFilter customFilterColumn ->
            CustomFilter (CustomFilterColumn.toggleCollapse customFilterColumn)

        Dated datedColumn ->
            Dated (DatedColumn.toggleCollapse datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.toggleCollapse namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.toggleCollapse otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.toggleCollapse undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.toggleCollapse untaggedColumn)


updateName : String -> Column -> Column
updateName newName column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.updateName newName completedColumn)

        CustomFilter customFilterColumn ->
            CustomFilter (CustomFilterColumn.updateName newName customFilterColumn)

        Dated datedColumn ->
            Dated (DatedColumn.updateName newName datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.updateName newName namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.updateName newName otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.updateName newName undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.updateName newName untaggedColumn)


updateOtherTags : (OtherTagsColumn -> OtherTagsColumn) -> Column -> Column
updateOtherTags fn column =
    case column of
        OtherTags otherTagsColumn ->
            OtherTags <| fn otherTagsColumn

        _ ->
            column



-- PRIVATE


tagsToHide : Column -> List String
tagsToHide column =
    case column of
        Completed completedColumn ->
            CompletedColumn.tagsToHide completedColumn

        CustomFilter customFilterColumn ->
            CustomFilterColumn.tagsToHide customFilterColumn

        Dated datedColumn ->
            DatedColumn.tagsToHide datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.tagsToHide namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.tagsToHide otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.tagsToHide undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.tagsToHide untaggedColumn


toList : Column -> List TaskItem
toList column =
    case column of
        Completed completedColumn ->
            CompletedColumn.toList completedColumn

        CustomFilter customFilterColumn ->
            CustomFilterColumn.toList customFilterColumn

        Dated datedColumn ->
            DatedColumn.toList datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.toList namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.toList otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.toList undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.toList untaggedColumn
