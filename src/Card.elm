module Card exposing
    ( Card
    , Highlight(..)
    , descendantTasks
    , editButtonId
    , filePath
    , fromTaskItem
    , highlight
    , id
    , markdownWithIds
    , notesId
    , taskItem
    , taskItemId
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Card
    = Card String TaskItem


type Highlight
    = HighlightNone
    | HighlightCritical
    | HighlightGood
    | HighlightImportant



-- CONSTRUCTION


fromTaskItem : String -> TaskItem -> Card
fromTaskItem =
    Card



-- INFO


highlight : TimeWithZone -> Card -> Highlight
highlight timeWithZone (Card _ item) =
    let
        datestamp : Date
        datestamp =
            TimeWithZone.toDate timeWithZone
    in
    case ( TaskItem.isCompleted item, TaskItem.due item ) of
        ( False, Just dueDate ) ->
            if datestamp == dueDate then
                HighlightImportant

            else if Date.toRataDie datestamp > Date.toRataDie dueDate then
                HighlightCritical

            else if Date.toRataDie datestamp < Date.toRataDie dueDate then
                HighlightGood

            else
                HighlightNone

        _ ->
            HighlightNone


id : Card -> String
id ((Card idPrefix _) as card) =
    idPrefix ++ ":" ++ taskItemId card


editButtonId : Card -> String
editButtonId card =
    id card ++ ":editButton"


filePath : Card -> String
filePath (Card _ item) =
    TaskItem.filePath item


markdownWithIds : Card -> List { id : String, markdown : String }
markdownWithIds card =
    let
        item : TaskItem
        item =
            taskItem card

        subtaskMarkdownWithId : ( String, TaskItem ) -> { id : String, markdown : String }
        subtaskMarkdownWithId ( subtaskId, subtask ) =
            { id = subtaskId
            , markdown = TaskItem.title subtask
            }

        subtasksWithIds : List { id : String, markdown : String }
        subtasksWithIds =
            card
                |> descendantTasks
                |> List.map subtaskMarkdownWithId

        notesWithId : List { id : String, markdown : String }
        notesWithId =
            if TaskItem.hasNotes item then
                [ { id = notesId card
                  , markdown = TaskItem.notes item
                  }
                ]

            else
                []

        markdownWithId : Card -> List { id : String, markdown : String }
        markdownWithId c =
            [ { id = id c
              , markdown = TaskItem.title item
              }
            ]
    in
    card
        |> markdownWithId
        |> List.append subtasksWithIds
        |> List.append notesWithId


notesId : Card -> String
notesId card =
    id card ++ ":notes"


descendantTasks : Card -> List ( String, TaskItem )
descendantTasks (Card idPrefix item) =
    TaskItem.descendantTasks item
        |> List.map (\sub -> ( idPrefix ++ ":" ++ TaskItem.id sub, sub ))


taskItem : Card -> TaskItem
taskItem (Card _ item) =
    item


taskItemId : Card -> String
taskItemId (Card _ item) =
    TaskItem.id item
