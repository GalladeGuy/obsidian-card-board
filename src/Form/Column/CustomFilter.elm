module Form.Column.CustomFilter exposing
    ( CustomFilterColumnForm
    , init
    , safeDecoder
    , updateFilterExpression
    , updateName
    )

import Column.CustomFilter as CustomFilterColumn exposing (CustomFilterColumn)
import Form.SafeDecoder as SD


-- TYPES


type alias CustomFilterColumnForm =
    { name : String
    , filterExpression : String
    }


-- CONSTRUCTION


init : CustomFilterColumn -> CustomFilterColumnForm
init customFilterColumn =
    { name = CustomFilterColumn.name customFilterColumn
    , filterExpression = CustomFilterColumn.filterExpression customFilterColumn
    }


-- DECODER


safeDecoder : SD.Decoder CustomFilterColumnForm CustomFilterColumn
safeDecoder =
    SD.map2 CustomFilterColumn.init
        safeNameDecoder
        safeFilterExpressionDecoder


-- MODIFICATION


updateName : String -> CustomFilterColumnForm -> CustomFilterColumnForm
updateName newName form =
    { form | name = newName }


updateFilterExpression : String -> CustomFilterColumnForm -> CustomFilterColumnForm
updateFilterExpression newFilterExpression form =
    { form | filterExpression = newFilterExpression }


-- PRIVATE


safeNameDecoder : SD.Decoder CustomFilterColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name


safeFilterExpressionDecoder : SD.Decoder CustomFilterColumnForm String
safeFilterExpressionDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .filterExpression
