module Page.Helper.MultiSelect exposing
    ( Config
    , Model
    , Msg
    , SelectionItem
    , deleteHighlightedItem
    , init
    , recieveItems
    , selectedItems
    , update
    , updateGrouper
    , updateSelectedItems
    , view
    )

import AssocList as Dict exposing (Dict)
import Browser.Dom as Dom
import Fuzzy
import Html exposing (Html)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events as Events
import Process
import Task



-- MODEL


type Model msg a
    = Ready (Config msg a) (Status a)
    | SettingItems (Config msg a) (Status a)
    | ReceivedItems (Config msg a) (Status a)


type alias Config msg a =
    { delayMs : Float
    , tagger : Msg msg a -> msg
    , fetchMsg : Int -> String -> msg
    , notFoundText : String
    , grouper : List (SelectionItem a) -> List ( String, List (SelectionItem a) )
    , selectedItemLabel : a -> String
    }


type alias Status a =
    { selectedItems : Dict String a
    , highlightedItem : String
    , searchTerm : String
    , page : Int
    , dropdownItems : List (SelectionItem a)
    , showDropDown : Bool
    , itemWasPressed : Bool
    }


type alias SelectionItem a =
    { label : String
    , value : a
    }


init : Config msg a -> Dict String a -> Model msg a
init initialConfig selected =
    Ready initialConfig
        { selectedItems = selected
        , highlightedItem = ""
        , searchTerm = ""
        , page = 0
        , dropdownItems = []
        , showDropDown = False
        , itemWasPressed = False
        }



-- INFO


config : Model msg a -> Config msg a
config model =
    case model of
        Ready conf _ ->
            conf

        SettingItems conf _ ->
            conf

        ReceivedItems conf _ ->
            conf


tagger : Model msg a -> (Msg msg a -> msg)
tagger =
    .tagger << config


status : Model msg a -> Status a
status model =
    case model of
        Ready _ selectStatus ->
            selectStatus

        SettingItems _ selectStatus ->
            selectStatus

        ReceivedItems _ selectStatus ->
            selectStatus


selectedItems : Model msg a -> Dict String a
selectedItems =
    .selectedItems << status


highlightedItem : Model msg a -> String
highlightedItem =
    .highlightedItem << status


searchTerm : Model msg a -> String
searchTerm =
    .searchTerm << status


page : Model msg a -> Int
page =
    .page << status


dropdownItems : Model msg a -> List (SelectionItem a)
dropdownItems =
    .dropdownItems << status


showDropDown : Model msg a -> Bool
showDropDown =
    .showDropDown << status


itemWasPressed : Model msg a -> Bool
itemWasPressed =
    .itemWasPressed << status



-- TRANSFORM


mapConfig : (Config msg a -> Config msg a) -> Model msg a -> Model msg a
mapConfig fn model =
    case model of
        Ready conf selectStatus ->
            Ready (fn conf) selectStatus

        SettingItems conf selectStatus ->
            SettingItems (fn conf) selectStatus

        ReceivedItems conf selectStatus ->
            ReceivedItems (fn conf) selectStatus


mapStatus : (Status a -> Status a) -> Model msg a -> Model msg a
mapStatus transform model =
    case model of
        Ready conf selectStatus ->
            Ready conf (transform selectStatus)

        SettingItems conf selectStatus ->
            SettingItems conf (transform selectStatus)

        ReceivedItems conf selectStatus ->
            ReceivedItems conf (transform selectStatus)


recieveItems : Model msg a -> List (SelectionItem a) -> Model msg a
recieveItems model items =
    case model of
        Ready _ _ ->
            model

        SettingItems conf selectStatus ->
            ReceivedItems
                conf
                { selectStatus
                    | dropdownItems = items
                    , itemWasPressed = False
                    , showDropDown = True
                }

        ReceivedItems _ _ ->
            model


addToSelected : SelectionItem a -> Model msg a -> Model msg a
addToSelected item model =
    mapStatus
        (\s ->
            { s
                | selectedItems =
                    Dict.insert item.label item.value s.selectedItems
            }
        )
        model
        |> mapStatus (\s -> { s | itemWasPressed = False })


deleteHighlightedItem : Model msg a -> Model msg a
deleteHighlightedItem model =
    mapStatus
        (\s ->
            { s
                | selectedItems =
                    Dict.remove (highlightedItem model) s.selectedItems
                , highlightedItem = ""
            }
        )
        model


updateGrouper : (List (SelectionItem a) -> List ( String, List (SelectionItem a) )) -> Model msg a -> Model msg a
updateGrouper newGrouper model =
    mapConfig (\c -> { c | grouper = newGrouper }) model


updateSelectedItems : Dict String a -> Model msg a -> Model msg a
updateSelectedItems selectedItems_ model =
    mapStatus (\s -> { s | selectedItems = selectedItems_ }) model



-- UPDATE


type Msg msg a
    = DelayedRequest String
    | FocusGained
    | FocusLost
    | ItemMouseDown
    | ItemSelected (SelectionItem a)
    | ChosenItemClicked String
    | SearchTermChanged String
    | SelectClicked
    | SendRequest


update : Msg msg a -> Model msg a -> ( Model msg a, Cmd msg )
update msg model =
    case msg of
        ItemSelected item ->
            ( addToSelected item model
            , setFocus <| tagger model
            )

        ChosenItemClicked label ->
            ( mapStatus (\s -> { s | highlightedItem = label }) model
            , Cmd.none
            )

        DelayedRequest delayedTerm ->
            if delayedTerm == searchTerm model then
                ( SettingItems
                    (config model)
                    { selectedItems = selectedItems model
                    , highlightedItem = highlightedItem model
                    , searchTerm = delayedTerm
                    , dropdownItems = dropdownItems model
                    , showDropDown = showDropDown model
                    , itemWasPressed = False
                    , page = 0
                    }
                , performRequest <| tagger model
                )

            else
                ( model, Cmd.none )

        FocusGained ->
            if itemWasPressed model && not (Dict.isEmpty (selectedItems model)) then
                ( model
                    |> mapStatus (\s -> { s | itemWasPressed = False })
                , Cmd.none
                )

            else if List.isEmpty (dropdownItems model) then
                primeRequest "" model

            else
                ( model
                    |> mapStatus (\s -> { s | itemWasPressed = False, showDropDown = True })
                , Cmd.none
                )

        FocusLost ->
            if itemWasPressed model then
                ( model, Cmd.none )

            else
                reset model

        ItemMouseDown ->
            ( mapStatus (\s -> { s | itemWasPressed = True }) model
            , Cmd.none
            )

        SearchTermChanged newSearchTerm ->
            ( mapStatus (\s -> { s | searchTerm = newSearchTerm }) model
            , Cmd.none
            )

        SelectClicked ->
            ( model
            , setFocus <| tagger model
            )

        SendRequest ->
            let
                fetchMsg : Int -> String -> msg
                fetchMsg =
                    (.fetchMsg << config) model
            in
            ( model
            , fetchMsg (page model) (searchTerm model)
                |> Task.succeed
                |> Task.perform identity
            )


setFocus : (Msg msg a -> msg) -> Cmd msg
setFocus msgTagger =
    Dom.focus "multiSelectInput"
        |> Task.attempt (always FocusGained >> msgTagger)


reset : Model msg a -> ( Model msg a, Cmd msg )
reset model =
    let
        newModel : Model msg a
        newModel =
            case model of
                Ready _ _ ->
                    model

                SettingItems conf selectStatus ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , highlightedItem = ""
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , showDropDown = False
                        , itemWasPressed = False
                        }

                ReceivedItems conf selectStatus ->
                    ReceivedItems conf
                        { selectedItems = selectStatus.selectedItems
                        , highlightedItem = ""
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = selectStatus.dropdownItems
                        , showDropDown = False
                        , itemWasPressed = False
                        }
    in
    ( newModel, Cmd.none )


primeRequest : String -> Model msg a -> ( Model msg a, Cmd msg )
primeRequest searchFor model =
    ( SettingItems
        (config model)
        { selectedItems = selectedItems model
        , highlightedItem = ""
        , searchTerm = searchFor
        , dropdownItems = dropdownItems model
        , showDropDown = showDropDown model
        , itemWasPressed = False
        , page = page model
        }
    , delayedSend ((.delayMs << config) model) (tagger model <| DelayedRequest searchFor)
    )


performRequest : (Msg msg a -> msg) -> Cmd msg
performRequest msgTagger =
    (SendRequest |> msgTagger)
        |> Task.succeed
        |> Task.perform identity


delayedSend : Float -> msg -> Cmd msg
delayedSend milli msg =
    Process.sleep milli
        |> Task.perform (\_ -> msg)



-- VIEW


view : Model msg a -> Html msg
view model =
    Html.div []
        [ Html.div
            [ class "multiselect-items"
            , Events.onClick (SelectClicked |> tagger model)
            ]
            (chosenItems (config model) (selectedItems model) (highlightedItem model) (tagger model)
                ++ [ input (tagger model) (searchTerm model) ]
            )
        , dropDownMenu model
        ]


chosenItems : Config msg a -> Dict String a -> String -> (Msg msg a -> msg) -> List (Html msg)
chosenItems conf selected highlighted msgTagger =
    selected
        |> Dict.foldr
            (\label filter acc ->
                chosenItem conf highlighted msgTagger filter label :: acc
            )
            []


chosenItem : Config msg a -> String -> (Msg msg a -> msg) -> a -> String -> Html msg
chosenItem conf highlighted msgTagger item itemText =
    let
        itemClass : String
        itemClass =
            if itemText == highlighted then
                "multiselect-item selected"

            else
                "multiselect-item"
    in
    Html.div
        [ class itemClass
        , Events.onMouseDown (ItemMouseDown |> msgTagger)
        , Events.onClick (ChosenItemClicked itemText |> msgTagger)
        ]
        [ Html.span [ class "multiselect-item-key" ]
            [ Html.text (conf.selectedItemLabel item) ]
        , Html.span [ class "multiselect-item-value" ]
            [ Html.text itemText ]
        ]


input : (Msg msg a -> msg) -> String -> Html msg
input msgTagger currentSearchTerm =
    Html.div []
        [ Html.input
            [ id "multiSelectInput"
            , class "multiselect-input"
            , type_ "text"
            , value currentSearchTerm
            , Events.onInput (SearchTermChanged >> msgTagger)
            , Events.onBlur (FocusLost |> msgTagger)
            ]
            []
        ]


dropDownMenu : Model msg a -> Html msg
dropDownMenu model =
    case model of
        Ready conf selectStatus ->
            itemsOrDefault conf selectStatus (Html.div [] [])

        SettingItems conf selectStatus ->
            itemsOrDefault conf selectStatus (Html.div [] [])

        ReceivedItems conf selectStatus ->
            itemsOrDefault
                conf
                selectStatus
                (wrapInDropDown <| showStatic conf.notFoundText)


itemsOrDefault : Config msg a -> Status a -> Html msg -> Html msg
itemsOrDefault conf selectStatus element =
    case ( selectStatus.showDropDown, selectStatus.dropdownItems ) of
        ( False, _ ) ->
            Html.text ""

        ( True, [] ) ->
            element

        ( True, _ ) ->
            selectStatus.dropdownItems
                |> fuzzyMatch selectStatus.searchTerm
                |> showSelectionSections conf
                |> wrapInDropDown


fuzzyMatch : String -> List (SelectionItem a) -> List (SelectionItem a)
fuzzyMatch needle selectionItems =
    let
        score : SelectionItem a -> Maybe ( Int, SelectionItem a )
        score selectionItem =
            let
                result : Fuzzy.Result
                result =
                    Fuzzy.match [ Fuzzy.addPenalty 10, Fuzzy.removePenalty 100 ] [ "/", " ", "\\" ] (String.toLower needle) (String.toLower selectionItem.label)
            in
            if String.isEmpty needle then
                Just ( 0, selectionItem )

            else if result.score > 3000 then
                Nothing

            else
                Just ( result.score, selectionItem )
    in
    selectionItems
        |> List.filterMap score
        |> List.sortBy (\i -> String.toLower (Tuple.second i).label)
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


wrapInDropDown : List (Html msg) -> Html msg
wrapInDropDown content =
    Html.div [ class "suggestion-container" ] content


showStatic : String -> List (Html msg)
showStatic content =
    [ Html.div []
        [ Html.text content ]
    ]


showSelectionSections : Config msg a -> List (SelectionItem a) -> List (Html msg)
showSelectionSections conf selections =
    selections
        |> conf.grouper
        |> List.map (showSelectionSection conf)


showSelectionSection : Config msg a -> ( String, List (SelectionItem a) ) -> Html msg
showSelectionSection conf ( title, selections ) =
    Html.div [ class "suggestion-section" ]
        [ Html.div [ class "suggestion-section-heading" ]
            [ Html.text title ]
        , Html.div []
            (showSelections conf selections)
        ]


showSelections : Config msg a -> List (SelectionItem a) -> List (Html msg)
showSelections conf selections =
    selections
        |> List.take 5
        |> List.map
            (\s ->
                Html.div
                    [ class "suggestion-item"
                    , Events.onMouseDown (ItemMouseDown |> conf.tagger)
                    , Events.onClick <| (ItemSelected s |> conf.tagger)
                    ]
                    [ Html.text s.label ]
            )



-- SCROLLING
