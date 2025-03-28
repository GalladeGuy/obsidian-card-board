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
import TagList exposing (TagList)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode


-- TYPES


type CustomFilterColumn
    = CustomFilterColumn Config (List String) TaskList (TaskItem -> Date -> Bool)


type alias Config =
    { collapsed : Bool
    , name : String
    , expression : String
    }


type alias TokenState =
    { current : String
    , tokens : List String
    , inEscape : Bool
    , quoteChar : Maybe Char
    }


-- CONSTRUCTION


init : String -> String -> CustomFilterColumn
init name_ expression_ =
    let
        filterFunction : TaskItem -> Date -> Bool
        filterFunction = compileFilterExpression expression_
    in
    CustomFilterColumn 
        { collapsed = False
        , name = name_
        , expression = expression_
        } 
        [] 
        TaskList.empty
        filterFunction


-- DECODE / ENCODE


decoder : TsDecode.Decoder CustomFilterColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
        |> TsDecode.required "expression" TsDecode.string
    )
        |> TsDecode.map (\c -> 
            let
                filterFunction = compileFilterExpression c.expression
            in
            CustomFilterColumn c [] TaskList.empty filterFunction)


encoder : TsEncode.Encoder CustomFilterColumn
encoder =
    TsEncode.map config configEncoder


-- INFO


containsTask : String -> CustomFilterColumn -> Bool
containsTask taskId (CustomFilterColumn _ _ tl _) =
    TaskList.containsTask taskId tl


isCollapsed : CustomFilterColumn -> Bool
isCollapsed (CustomFilterColumn c _ _ _) =
    c.collapsed


name : CustomFilterColumn -> String
name (CustomFilterColumn c _ _ _) =
    c.name


filterExpression : CustomFilterColumn -> String
filterExpression (CustomFilterColumn c _ _ _) =
    c.expression


tagsToHide : CustomFilterColumn -> List String
tagsToHide (CustomFilterColumn _ tth _ _) =
    tth


toList : CustomFilterColumn -> List TaskItem
toList (CustomFilterColumn _ _ tl _) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


-- MODIFICATION


addTaskItem : Date -> TaskItem -> CustomFilterColumn -> ( CustomFilterColumn, PlacementResult )
addTaskItem currentDate taskItem ((CustomFilterColumn c tth tl filterFn) as column) =
    if filterFn taskItem currentDate then
        ( CustomFilterColumn c tth (TaskList.add taskItem tl) filterFn, PlacementResult.Placed )
    else
        ( column, PlacementResult.DoesNotBelong )


setCollapse : Bool -> CustomFilterColumn -> CustomFilterColumn
setCollapse isCollapsed_ (CustomFilterColumn c tth tl filterFn) =
    CustomFilterColumn { c | collapsed = isCollapsed_ } tth tl filterFn


setTagsToHide : List String -> CustomFilterColumn -> CustomFilterColumn
setTagsToHide tags (CustomFilterColumn c _ tl filterFn) =
    CustomFilterColumn c tags tl filterFn


toggleCollapse : CustomFilterColumn -> CustomFilterColumn
toggleCollapse (CustomFilterColumn c tth tl filterFn) =
    CustomFilterColumn { c | collapsed = not c.collapsed } tth tl filterFn


updateName : String -> CustomFilterColumn -> CustomFilterColumn
updateName newName (CustomFilterColumn c tth tl filterFn) =
    CustomFilterColumn { c | name = newName } tth tl filterFn


updateExpression : String -> CustomFilterColumn -> CustomFilterColumn
updateExpression newExpression (CustomFilterColumn c tth tl _) =
    let
        newFilterFn = compileFilterExpression newExpression
    in
    CustomFilterColumn { c | expression = newExpression } tth tl newFilterFn


-- PRIVATE


tokenize : String -> List String
tokenize str =
    let
        addToken : String -> List String -> List String
        addToken curr tokens =
            if String.isEmpty curr then
                tokens
            else
                curr :: tokens

        finalizeTokens : TokenState -> List String
        finalizeTokens state =
            case state.quoteChar of
                Just _ ->
                    []  -- Unclosed quote, return empty list
                Nothing ->
                    addToken state.current state.tokens
                
        processEscape : Char -> TokenState -> TokenState
        processEscape c state =
            if state.inEscape then
                case c of
                    'n' -> { state | current = state.current ++ "\n", inEscape = False }
                    't' -> { state | current = state.current ++ "\t", inEscape = False }
                    '\'' -> { state | current = state.current ++ "'", inEscape = False }
                    '"' -> { state | current = state.current ++ "\"", inEscape = False }
                    '\\' -> { state | current = state.current ++ "\\", inEscape = False }
                    _ -> { state | current = state.current ++ String.fromChar c, inEscape = False }
            else if c == '\\' then
                { state | inEscape = True }
            else
                processChar c state

        processChar : Char -> TokenState -> TokenState
        processChar c state =
            case state.quoteChar of
                Just quote ->
                    if c == quote && not state.inEscape then
                        { state | current = "", tokens = addToken state.current state.tokens, quoteChar = Nothing }
                    else if c == '\\' then
                        { state | inEscape = True }
                    else
                        { state | current = state.current ++ String.fromChar c }
                Nothing ->
                    if c == '"' || c == '\'' then
                        { state | current = "", quoteChar = Just c }
                    else if c == '(' || c == ')' then
                        { state | current = "", tokens = String.fromChar c :: addToken state.current state.tokens }
                    else if isWhitespace c then
                        { state | current = "", tokens = addToken state.current state.tokens }
                    else
                        { state | current = state.current ++ String.fromChar c }

        initialState =
            { current = ""
            , tokens = []
            , inEscape = False
            , quoteChar = Nothing
            }
    in
    str
        |> String.toList
        |> List.foldl processEscape initialState
        |> finalizeTokens
        |> List.reverse


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'


config : CustomFilterColumn -> Config
config (CustomFilterColumn c _ _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        , TsEncode.required "expression" .expression TsEncode.string
        ]


-- compileFilterExpression transforms a filter expression string into a function
-- that evaluates a task item against the filter criteria
compileFilterExpression : String -> (TaskItem -> Date -> Bool)
compileFilterExpression expression =
    if String.isEmpty expression then
        -- Empty filter matches all tasks
        \_ _ -> True
    else
        let
            -- Pre-compute the tokens - this happens once per function creation
            -- due to Elm's lazy evaluation of let bindings
            tokenized = tokenize expression
            
            -- Create a function that uses these pre-computed tokens
            evaluateFilter taskItem currentDate =
                let
                    
                    -- Evaluation functions that depend on taskItem and currentDate
                    hasTag tag =
                        TaskItem.hasThisTag (String.dropLeft 1 tag) taskItem
                    
                    parseDateToken : String -> Result String Date
                    parseDateToken token =
                        case token of
                            "today" ->
                                Ok currentDate
                            
                            "yesterday" ->
                                Ok (Date.add Date.Days -1 currentDate)
                            
                            "tomorrow" ->
                                Ok (Date.add Date.Days 1 currentDate)
                            
                            _ ->
                                Date.fromIsoString token
                
                    beforeDate dateStr =
                        case TaskItem.due taskItem of
                            Nothing -> False
                            Just itemDate ->
                                case parseDateToken dateStr of
                                    Ok date -> Date.compare itemDate date == LT
                                    Err _ -> False
                                
                    byDate dateStr =
                        case TaskItem.due taskItem of
                            Nothing -> False
                            Just itemDate ->
                                case parseDateToken dateStr of
                                    Ok date -> Date.compare itemDate date == LT || Date.compare itemDate date == EQ
                                    Err _ -> False
                                
                    onDate dateStr =
                        case TaskItem.due taskItem of
                            Nothing -> False
                            Just itemDate ->
                                case parseDateToken dateStr of
                                    Ok date -> Date.compare itemDate date == EQ
                                    Err _ -> False
                
                    fromDate dateStr =
                        case TaskItem.due taskItem of
                            Nothing -> False
                            Just itemDate ->
                                case parseDateToken dateStr of
                                    Ok date -> Date.compare itemDate date == GT || Date.compare itemDate date == EQ
                                    Err _ -> False
        
                    afterDate dateStr =
                        case TaskItem.due taskItem of
                            Nothing -> False
                            Just itemDate ->
                                case parseDateToken dateStr of
                                    Ok date -> Date.compare itemDate date == GT
                                    Err _ -> False
        
                    isCompleted =
                        TaskItem.isCompleted taskItem
        
                    containsText : String -> String -> Bool
                    containsText searchStr text =
                        String.contains searchStr text
        
                    containsInTags : String -> Bool
                    containsInTags searchStr =
                        List.any (String.contains searchStr) (TagList.toStrings (TaskItem.tags taskItem))
        
                    getFilename : String -> String
                    getFilename path =
                        path
                            |> String.split "/"
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault path
                        
                    -- Parse expression with C operator precedence
                    parseExpr : List String -> ( Bool, List String )
                    parseExpr tokens =
                        parseOr tokens
        
                    -- Parse OR expressions
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
        
                    -- Parse NOT expressions
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
                        
                            "by" :: date :: rest ->
                                ( byDate date, rest )
                            
                            "on" :: date :: rest ->
                                ( onDate date, rest )
                            
                            "from" :: date :: rest ->
                                ( fromDate date, rest )
                            
                            "after" :: date :: rest ->
                                ( afterDate date, rest )
                            
                            "completed" :: rest ->
                                ( isCompleted, rest )
                            
                            "contents" :: "contains" :: searchStr :: rest ->
                                ( containsText searchStr (TaskItem.originalLine taskItem), rest )
        
                            "title" :: "contains" :: searchStr :: rest ->
                                ( containsText searchStr (TaskItem.title taskItem), rest )
        
                            "tags" :: "contains" :: searchStr :: rest ->
                                ( containsInTags searchStr, rest )
        
                            "notes" :: "contains" :: searchStr :: rest ->
                                ( containsText searchStr (TaskItem.notes taskItem), rest )
        
                            "path" :: "contains" :: searchStr :: rest ->
                                ( containsText searchStr (TaskItem.filePath taskItem), rest )
        
                            "filename" :: "contains" :: searchStr :: rest ->
                                ( containsText searchStr (getFilename (TaskItem.filePath taskItem)), rest )
        
                            "contains" :: searchStr :: rest ->
                                ( containsText searchStr (TaskItem.originalLine taskItem), rest )
        
                            token :: rest ->
                                if String.startsWith "#" token then
                                    ( hasTag token, rest )
                                else
                                    ( False, [] )  -- Invalid token
                in
                    case parseExpr tokenized of
                        ( result, [] ) -> result
                        ( _, _ ) -> False  -- Invalid expression or extra tokens
        in
        -- Return the filter function that uses the pre-computed tokens
        evaluateFilter
