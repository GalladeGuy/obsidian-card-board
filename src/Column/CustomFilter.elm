module Column.CustomFilter exposing
    ( CustomFilterColumn
    , addTaskItem
    , containsTask
    , decoder
    , encoder
    , filterExpression
    , init
    , isCollapsed
    , name
    , setCollapse
    , setTagsToHide
    , tagsToHide
    , toList
    , toggleCollapse
    , updateExpression
    , updateName
    )

import Date exposing (Date)
import DefaultColumnNames exposing (DefaultColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode


-- TYPES


type CustomFilterColumn
    = CustomFilterColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    , expression : String
    }


-- CONSTRUCTION


init : String -> String -> CustomFilterColumn
init name_ expression_ =
    CustomFilterColumn 
        { collapsed = False
        , name = name_
        , expression = expression_
        } 
        [] 
        TaskList.empty


-- DECODE / ENCODE


decoder : TsDecode.Decoder CustomFilterColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
        |> TsDecode.required "expression" TsDecode.string
    )
        |> TsDecode.map (\c -> CustomFilterColumn c [] TaskList.empty)


encoder : TsEncode.Encoder CustomFilterColumn
encoder =
    TsEncode.map config configEncoder


-- INFO


containsTask : String -> CustomFilterColumn -> Bool
containsTask taskId (CustomFilterColumn _ _ tl) =
    TaskList.containsTask taskId tl


isCollapsed : CustomFilterColumn -> Bool
isCollapsed (CustomFilterColumn c _ _) =
    c.collapsed


name : CustomFilterColumn -> String
name (CustomFilterColumn c _ _) =
    c.name


filterExpression : CustomFilterColumn -> String
filterExpression (CustomFilterColumn c _ _) =
    c.expression


tagsToHide : CustomFilterColumn -> List String
tagsToHide (CustomFilterColumn _ tth _) =
    tth


toList : CustomFilterColumn -> List TaskItem
toList (CustomFilterColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


-- MODIFICATION


addTaskItem : TaskItem -> CustomFilterColumn -> ( CustomFilterColumn, PlacementResult )
addTaskItem taskItem ((CustomFilterColumn c tth tl) as column) =
    if matchesFilter (filterExpression column) taskItem then
        ( CustomFilterColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )
    else
        ( column, PlacementResult.DoesNotBelong )


setCollapse : Bool -> CustomFilterColumn -> CustomFilterColumn
setCollapse isCollapsed_ (CustomFilterColumn c tth tl) =
    CustomFilterColumn { c | collapsed = isCollapsed_ } tth tl


setTagsToHide : List String -> CustomFilterColumn -> CustomFilterColumn
setTagsToHide tags (CustomFilterColumn c _ tl) =
    CustomFilterColumn c tags tl


toggleCollapse : CustomFilterColumn -> CustomFilterColumn
toggleCollapse (CustomFilterColumn c tth tl) =
    CustomFilterColumn { c | collapsed = not c.collapsed } tth tl


updateName : String -> CustomFilterColumn -> CustomFilterColumn
updateName newName (CustomFilterColumn c tth tl) =
    CustomFilterColumn { c | name = newName } tth tl


updateExpression : String -> CustomFilterColumn -> CustomFilterColumn
updateExpression newExpression (CustomFilterColumn c tth tl) =
    CustomFilterColumn { c | expression = newExpression } tth tl


-- PRIVATE


config : CustomFilterColumn -> Config
config (CustomFilterColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        , TsEncode.required "expression" .expression TsEncode.string
        ]


matchesFilter : String -> TaskItem -> Bool
matchesFilter expression taskItem =
    let
        hasTag tag =
            TaskItem.hasThisTag (String.dropLeft 1 tag) taskItem
            
        beforeDate dateStr =
            case TaskItem.due taskItem of
                Nothing -> False
                Just itemDate ->
                    case Date.fromIsoString dateStr of
                        Ok date -> Date.compare itemDate date == LT
                        Err _ -> False
                        
        afterDate dateStr =
            case TaskItem.due taskItem of
                Nothing -> False
                Just itemDate ->
                    case Date.fromIsoString dateStr of
                        Ok date -> Date.compare itemDate date == GT
                        Err _ -> False
                        
        onDate dateStr =
            case TaskItem.due taskItem of
                Nothing -> False
                Just itemDate ->
                    case Date.fromIsoString dateStr of
                        Ok date -> Date.compare itemDate date == EQ
                        Err _ -> False

        isCompleted =
            TaskItem.isCompleted taskItem

        tokenize : String -> List String
        tokenize str =
            let
                addToken : String -> List String -> List String
                addToken curr tokens =
                    if String.isEmpty curr then
                        tokens
                    else
                        curr :: tokens
                        
                processChar : Char -> ( String, List String ) -> ( String, List String )
                processChar c ( curr, tokens ) =
                    if c == '(' || c == ')' then
                        ( "", String.fromChar c :: addToken curr tokens )
                    else if isWhitespace c then
                        ( "", addToken curr tokens )
                    else
                        ( curr ++ String.fromChar c, tokens )
            in
            str
                |> String.toLower
                |> String.toList
                |> List.foldl processChar ( "", [] )
                |> (\( curr, tokens ) -> addToken curr tokens)
                |> List.reverse

        isWhitespace : Char -> Bool
        isWhitespace c =
            c == ' ' || c == '\t' || c == '\n' || c == '\r'

        -- Parse expression with C operator precedence
        parseExpr : List String -> ( Bool, List String )
        parseExpr tokens =
            parseOr tokens

        -- Parse OR expressions (lowest precedence)
        parseOr : List String -> ( Bool, List String )
        parseOr tokens =
            let
                ( leftResult, rest1 ) = parseAnd tokens
            in
            case rest1 of
                "or" :: rest2 ->
                    let
                        ( rightResult, rest3 ) = parseOr rest2
                    in
                    ( leftResult || rightResult, rest3 )
                _ ->
                    ( leftResult, rest1 )

        -- Parse AND expressions
        parseAnd : List String -> ( Bool, List String )
        parseAnd tokens =
            let
                ( leftResult, rest1 ) = parseNot tokens
            in
            case rest1 of
                "and" :: rest2 ->
                    let
                        ( rightResult, rest3 ) = parseAnd rest2
                    in
                    ( leftResult && rightResult, rest3 )
                _ ->
                    ( leftResult, rest1 )

        -- Parse NOT expressions (highest precedence)
        parseNot : List String -> ( Bool, List String )
        parseNot tokens =
            case tokens of
                "not" :: rest ->
                    let
                        ( result, remaining ) = parseNot rest
                    in
                    ( not result, remaining )
                _ ->
                    parseTerm tokens

        -- Parse individual terms and parentheses
        parseTerm : List String -> ( Bool, List String )
        parseTerm tokens =
            case tokens of
                [] -> 
                    ( False, [] )
                    
                "(" :: rest ->
                    let
                        ( result, remaining ) = parseOr rest
                    in
                    case remaining of
                        ")" :: rest2 -> ( result, rest2 )
                        _ -> ( False, [] )  -- Unmatched parenthesis
                        
                "before" :: date :: rest ->
                    ( beforeDate date, rest )
                    
                "after" :: date :: rest ->
                    ( afterDate date, rest )
                    
                "on" :: date :: rest ->
                    ( onDate date, rest )
                    
                "completed" :: rest ->
                    ( isCompleted, rest )
                    
                token :: rest ->
                    if String.startsWith "#" token then
                        ( hasTag token, rest )
                    else
                        ( False, [] )  -- Invalid token
    in
    case parseOr (tokenize expression) of
        ( result, [] ) -> result
        ( _, _ ) -> False  -- Invalid expression or extra tokens