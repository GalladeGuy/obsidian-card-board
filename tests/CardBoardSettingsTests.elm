module CardBoardSettingsTests exposing (suite)

import BoardConfig
import CardBoardSettings
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Semver
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ currentVersion
        , encodeDecode
        , updateTaskUpdateFormat
        ]


currentVersion : Test
currentVersion =
    describe "currentVersion"
        [ test "is 0.5.0" <|
            \() ->
                CardBoardSettings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.5.0"
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding settings"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleSettings
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Expect.equal (Ok exampleSettings)
        , test "fails if the version number is unsupported" <|
            \() ->
                { exampleSettings | version = Semver.version 0 0 0 [] [] }
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


updateTaskUpdateFormat : Test
updateTaskUpdateFormat =
    describe "updateTaskUpdateFormat"
        [ test "can update to be ObsidianCardBoard format" <|
            \() ->
                exampleGlobalSettings
                    |> CardBoardSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> Expect.equal { taskUpdateFormat = CardBoardSettings.ObsidianCardBoard }
        , test "can update to be ObsidianTasks format" <|
            \() ->
                exampleGlobalSettings
                    |> CardBoardSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> CardBoardSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> Expect.equal { taskUpdateFormat = CardBoardSettings.ObsidianTasks }
        , test "defaults to be ObsidianCardBoard if the string is not recognised" <|
            \() ->
                exampleGlobalSettings
                    |> CardBoardSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> CardBoardSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> CardBoardSettings.updateTaskUpdateFormat "xxxxxx"
                    |> Expect.equal { taskUpdateFormat = CardBoardSettings.ObsidianCardBoard }
        ]



-- HELPERS


exampleSettings : CardBoardSettings.Settings
exampleSettings =
    { boardConfigs =
        [ BoardConfig.TagBoardConfig BoardConfigHelpers.exampleTagBoardConfig
        , BoardConfig.DateBoardConfig BoardConfigHelpers.exampleDateBoardConfig
        ]
    , globalSettings = exampleGlobalSettings
    , version = CardBoardSettings.currentVersion
    }


exampleGlobalSettings : CardBoardSettings.GlobalSettings
exampleGlobalSettings =
    { taskUpdateFormat = CardBoardSettings.ObsidianTasks }
