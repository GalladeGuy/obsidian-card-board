module DateBoardConfig exposing
    ( DateBoardConfig
    , decoder_v_0_10_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_9_0
    , default
    , encoder
    )

import CollapsedColumns exposing (CollapsedColumns)
import ColumnConfig exposing (ColumnConfig)
import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedConfig)
import ColumnConfigs exposing (ColumnConfigs)
import Filter exposing (Filter, Polarity, Scope)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DateBoardConfig =
    { collapsedColumns : CollapsedColumns
    , columnConfigs : ColumnConfigs
    , completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
    , includeUndated : Bool
    , showFilteredTags : Bool
    , title : String
    }


default : DateBoardConfig
default =
    { collapsedColumns = CollapsedColumns.init
    , columnConfigs = ColumnConfigs.defaultForDateBoard
    , completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , includeUndated = True
    , showFilteredTags = True
    , title = ""
    }



-- SERIALIZE


encoder : TsEncode.Encoder DateBoardConfig
encoder =
    TsEncode.object
        [ TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeUndated" .includeUndated TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        , TsEncode.required "collapsedColumns" .collapsedColumns CollapsedColumns.encoder
        ]


populateColumms : DateBoardConfig -> DateBoardConfig
populateColumms config =
    let
        dateColumns =
            [ ColumnConfig.todayColumn
            , ColumnConfig.tomorrowColumn
            , ColumnConfig.futureColumn
            ]

        undated =
            if config.includeUndated then
                [ ColumnConfig.defaultUndated ]

            else
                []

        completed =
            if config.completedCount > 0 then
                [ ColumnConfig.completed config.completedCount ]

            else
                []
    in
    { config | columnConfigs = ColumnConfigs.fromList (undated ++ dateColumns ++ completed) }


decoder_v_0_10_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_10_0 =
    decoder_v_0_9_0
        |> TsDecode.map populateColumms


decoder_v_0_9_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_9_0 =
    (TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.field "collapsedColumns" CollapsedColumns.decoder)
        |> TsDecode.andMap (TsDecode.succeed ColumnConfigs.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
    )
        |> TsDecode.map populateColumms


decoder_v_0_5_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_5_0 =
    (TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed ColumnConfigs.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
    )
        |> TsDecode.map populateColumms


decoder_v_0_4_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_4_0 =
    (TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed ColumnConfigs.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
    )
        |> TsDecode.map populateColumms


decoder_v_0_3_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_3_0 =
    (TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed ColumnConfigs.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
    )
        |> TsDecode.map populateColumms


decoder_v_0_2_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_2_0 =
    (TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed ColumnConfigs.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
    )
        |> TsDecode.map populateColumms


decoder_v_0_1_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_1_0 =
    (TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed ColumnConfigs.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
    )
        |> TsDecode.map populateColumms
