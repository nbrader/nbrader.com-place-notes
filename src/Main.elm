module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, textarea)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode
import Json.Encode
import List
import Maybe
import String

-- MAIN
main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
    { placeNotes : List PlaceNote -- List of notes in render order (oldest first, newest last/on top)
    , nextPlaceNoteId : Int -- Auto-incrementing ID for new notes
    , dragging : Maybe DraggingState -- Current drag state (note or camera)
    , cameraX : Int -- Camera X offset (positive = scrolled right)
    , cameraY : Int -- Camera Y offset (positive = scrolled down)
    , mode : Mode -- Current interaction mode (Move or Deletion)
    , inputText : String -- Current text in input field (synced with selected note if any)
    , selectedPlaceNoteId : Maybe Int -- ID of currently selected note (shown with red border)
    , jsonTextArea : String -- Raw textarea content for JSON import/export (allows editing)
    , pendingNewPlaceNote : Maybe String -- Text awaiting placement after pressing "Add"
    }

type Mode
    = MoveMode      -- Can drag notes and camera
    | DeletionMode  -- Click notes to delete them

type DraggingState
    = DraggingPlaceNote
        { draggedPlaceNoteId : Int -- ID of note being dragged
        , offsetX : Int            -- Mouse offset from note's left edge
        , offsetY : Int            -- Mouse offset from note's top edge
        , currentMouseX : Int      -- Current mouse X position (for live preview)
        , currentMouseY : Int      -- Current mouse Y position (for live preview)
        }
    | DraggingCamera
        { initialX : Int -- Mouse X when drag started
        , initialY : Int -- Mouse Y when drag started
        }

type alias PlaceNote =
    { id : Int     -- Unique identifier (never reused)
    , x : Int      -- X position in world space (not screen space)
    , y : Int      -- Y position in world space (not screen space)
    , width : Int  -- Note width in pixels (auto-calculated from text)
    , height : Int -- Note height in pixels (auto-calculated from text)
    , text : String -- Text content displayed on the note
    }

init : Model
init =
    let
        initialModel =
            { placeNotes = []
            , nextPlaceNoteId = 0
            , dragging = Nothing
            , cameraX = 0
            , cameraY = 0
            , mode = MoveMode
            , inputText = "Write a note here." -- Default text for a new PlaceNote
            , selectedPlaceNoteId = Nothing
            , jsonTextArea = "" -- Placeholder, will be set below
            , pendingNewPlaceNote = Nothing
            }
    in
    { initialModel | jsonTextArea = serializeModel initialModel }

-- UPDATE
type Msg
    = PrepareNewPlaceNote -- Start the process for placing a new PlaceNote
    | UpdatePlaceNoteText String -- Update text of the selected PlaceNote
    | MouseDown ( Int, Int )
    | MouseMove (Int, Int)
    | MouseUp
    | ToggleMode
    | NoOp
    | CopyTextFromSelectedPlaceNote -- Copy text from the selected PlaceNote
    | UpdateModelFromJson String

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleMode ->
            syncJsonTextArea <|
                case model.mode of
                    MoveMode ->
                        { model | mode = DeletionMode }
                    DeletionMode ->
                        { model | mode = MoveMode }

        PrepareNewPlaceNote ->
            syncJsonTextArea { model | pendingNewPlaceNote = Just model.inputText }

        UpdatePlaceNoteText newText ->
            let
                updatedPlaceNotes =
                    case model.selectedPlaceNoteId of
                        Just selectedId ->
                            List.map (updatePlaceNoteTextById selectedId newText) model.placeNotes
                        Nothing ->
                            model.placeNotes
            in
            syncJsonTextArea { model | inputText = newText, placeNotes = updatedPlaceNotes }

        MouseMove (mouseX, mouseY) ->
            -- O(1) operation: just update dragging state, don't touch the list
            -- Position calculated on-the-fly in view for massive performance gain
            case model.dragging of
                Just (DraggingPlaceNote dragState) ->
                    { model | dragging = Just (DraggingPlaceNote { dragState | currentMouseX = mouseX, currentMouseY = mouseY }) }
                Just (DraggingCamera { initialX, initialY }) ->
                    let
                        dx = mouseX - initialX
                        dy = mouseY - initialY
                    in
                    -- Update the camera position in the model
                    { model | cameraX = model.cameraX + dx, cameraY = model.cameraY + dy, dragging = Just (DraggingCamera { initialX = mouseX, initialY = mouseY }) }
                Nothing ->
                    model

        MouseUp ->
            -- Apply final position when dragging ends (O(n) once instead of O(n) per move)
            let
                updatedPlaceNotes =
                    case model.dragging of
                        Just (DraggingPlaceNote { draggedPlaceNoteId, offsetX, offsetY, currentMouseX, currentMouseY }) ->
                            List.map
                                (\placeNote ->
                                    if placeNote.id == draggedPlaceNoteId then
                                        { placeNote | x = currentMouseX - offsetX, y = currentMouseY - offsetY }
                                    else
                                        placeNote
                                )
                                model.placeNotes
                        _ ->
                            model.placeNotes
            in
            syncJsonTextArea { model | placeNotes = updatedPlaceNotes, dragging = Nothing }

        MouseDown (mouseX, mouseY) ->
            syncJsonTextArea <|
                case model.pendingNewPlaceNote of
                    Just textToPlace ->
                        let
                            ( noteWidth, noteHeight ) = calculateTextDimensions textToPlace
                            newPlaceNoteId = model.nextPlaceNoteId
                            newPlaceNote =
                                { id = newPlaceNoteId
                                , x = mouseX - model.cameraX - (noteWidth // 2)
                                , y = mouseY - model.cameraY - (noteHeight // 2)
                                , width = noteWidth
                                , height = noteHeight
                                , text = textToPlace
                                }
                        in
                        { model
                            | placeNotes = model.placeNotes ++ [ newPlaceNote ]
                            , nextPlaceNoteId = newPlaceNoteId + 1
                            , selectedPlaceNoteId = Just newPlaceNoteId
                            , pendingNewPlaceNote = Nothing
                            , inputText = textToPlace
                            , dragging = Nothing
                        }

                    Nothing ->
                        case model.mode of
                            DeletionMode ->
                                let
                                    clickedPlaceNote = findClickedPlaceNote model.placeNotes (mouseX - model.cameraX, mouseY - model.cameraY)
                                    updatedPlaceNotes =
                                        case clickedPlaceNote of
                                            Just placeNoteToBeRemoved ->
                                                List.filter (\placeNote -> placeNote.id /= placeNoteToBeRemoved.id) model.placeNotes

                                            Nothing ->
                                                model.placeNotes
                                in
                                { model | placeNotes = updatedPlaceNotes }

                            MoveMode ->
                                case findClickedPlaceNote model.placeNotes (mouseX - model.cameraX, mouseY - model.cameraY) of
                                    Just placeNote ->
                                        { model
                                            | dragging = Just
                                                (DraggingPlaceNote
                                                    { draggedPlaceNoteId = placeNote.id
                                                    , offsetX = mouseX - placeNote.x
                                                    , offsetY = mouseY - placeNote.y
                                                    , currentMouseX = mouseX
                                                    , currentMouseY = mouseY
                                                    }
                                                )
                                            , selectedPlaceNoteId = Just placeNote.id
                                            , inputText = placeNote.text  -- Populate input for editing
                                        }

                                    Nothing ->
                                        { model | dragging = Just (DraggingCamera { initialX = mouseX, initialY = mouseY }) }

        CopyTextFromSelectedPlaceNote ->
            syncJsonTextArea <|
                case model.selectedPlaceNoteId of
                    Just selectedId ->
                        let
                            selectedPlaceNoteText =
                                findPlaceNoteById selectedId model.placeNotes
                                    |> Maybe.map .text
                                    |> Maybe.withDefault model.inputText
                        in
                        { model | inputText = selectedPlaceNoteText }
                    Nothing ->
                        model
            
        UpdateModelFromJson jsonString ->
            -- Always update the textarea content so user can edit freely
            let
                modelWithUpdatedTextArea = { model | jsonTextArea = jsonString }
            in
            case Json.Decode.decodeString modelDecoder jsonString of
                Ok decodedModel ->
                    -- Valid JSON: apply the decoded model but preserve the textarea content
                    { decodedModel | jsonTextArea = jsonString }
                Err _ ->
                    -- Invalid JSON: keep old model but update textarea so user can continue editing
                    modelWithUpdatedTextArea
        
        NoOp ->
            model -- Do nothing

calculateTextHeight : String -> Int
calculateTextHeight text =
    let
        lines =
            case String.lines text of
                [] ->
                    [ "" ]
                ls ->
                    ls

        lineCount = List.length lines
    in
    max 50 (20 * lineCount + 10)

calculateTextDimensions : String -> ( Int, Int )
calculateTextDimensions text =
    let
        lines =
            case String.lines text of
                [] ->
                    [ "" ]
                ls ->
                    ls

        longestLineLength =
            lines
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 0

        width = max 80 (8 * longestLineLength + 16)
        height = calculateTextHeight text
    in
    ( width, height )

-- Update a placeNote's text and recalculate width if it matches the given ID
updatePlaceNoteTextById : Int -> String -> PlaceNote -> PlaceNote
updatePlaceNoteTextById targetId newText placeNote =
    if placeNote.id == targetId then
        let
            ( newWidth, newHeight ) = calculateTextDimensions newText
        in
        { placeNote | text = newText, width = newWidth, height = newHeight }
    else
        placeNote

-- Helper function to sync jsonTextArea when model changes from UI interactions
syncJsonTextArea : Model -> Model
syncJsonTextArea model =
    { model | jsonTextArea = serializeModel model }

-- Find a placeNote by ID (stops at first match for efficiency)
findPlaceNoteById : Int -> List PlaceNote -> Maybe PlaceNote
findPlaceNoteById targetId placeNotes =
    case placeNotes of
        [] ->
            Nothing
        placeNote :: rest ->
            if placeNote.id == targetId then
                Just placeNote
            else
                findPlaceNoteById targetId rest

-- Find a placeNote that has been clicked based on the mouse position
-- Searches in visual z-order: list is in render order, so reverse to check top-to-bottom
findClickedPlaceNote : List PlaceNote -> (Int, Int) -> Maybe PlaceNote
findClickedPlaceNote placeNotes (mouseX, mouseY) =
    -- Reverse to check from newest (end/top) to oldest (start/bottom)
    placeNotes
        |> List.reverse
        |> List.filter (\placeNote ->
            mouseX >= placeNote.x && mouseX <= placeNote.x + placeNote.width &&
            mouseY >= placeNote.y && mouseY <= placeNote.y + placeNote.height)
        |> List.head

-- VIEW
view : Model -> Html Msg
view model =
    let
        ( statusMessage, statusColor ) =
            case ( model.pendingNewPlaceNote, model.mode ) of
                ( Just _, _ ) ->
                    ( "Tap anywhere on the workspace to place your new note.", "#facc15" )

                ( Nothing, DeletionMode ) ->
                    ( "Deletion mode: tap a note to remove it.", "#f87171" )

                ( Nothing, MoveMode ) ->
                    ( "Move mode: drag notes or the background to pan.", "#38bdf8" )

        workspaceCursor =
            case ( model.mode, model.pendingNewPlaceNote ) of
                ( DeletionMode, _ ) ->
                    "not-allowed"

                ( _, Just _ ) ->
                    "copy"

                _ ->
                    "grab"

        modeLabel =
            case model.mode of
                MoveMode ->
                    "Move"

                DeletionMode ->
                    "Delete"

        workspaceAttributes =
            [ style "flex" "1"
            , style "position" "relative"
            , style "overflow" "hidden"
            , style "background-color" "#0b1220"
            , style "border-top" "1px solid #1f2937"
            , style "border-bottom" "1px solid #1f2937"
            , style "touch-action" "none"
            , style "cursor" workspaceCursor
            , Html.Events.on "mouseup" (Json.Decode.succeed MouseUp)
            , Html.Events.on "mousemove" mouseMoveDecoder
            , Html.Events.on "mousedown" mousePositionDecoder
            , Html.Events.on "touchend" (Json.Decode.succeed MouseUp)
            , Html.Events.on "touchmove" touchMoveDecoder
            , Html.Events.on "touchstart" touchStartDecoder
            , preventDragStart
            ]
    in
    div
        [ style "width" "100%"
        , style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "background-color" "#020617"
        , style "color" "#e2e8f0"
        , style "font-family" "Inter, system-ui, sans-serif"
        ]
        ([ div
                [ style "padding" "12px"
                , style "display" "flex"
                , style "gap" "8px"
                , style "flex-wrap" "wrap"
                , style "align-items" "center"
                ]
                [ input
                    [ type_ "text"
                    , value model.inputText
                    , onInput UpdatePlaceNoteText
                    , style "padding" "8px 12px"
                    , style "min-width" "220px"
                    , style "background-color" "#0f172a"
                    , style "color" "#e2e8f0"
                    , style "border" "1px solid #1f2937"
                    , style "border-radius" "6px"
                    ]
                    []
                , button
                    [ onClick PrepareNewPlaceNote
                    , style "padding" "8px 16px"
                    , style "background-color" "#2563eb"
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "6px"
                    , style "font-weight" "600"
                    ]
                    [ text (if model.pendingNewPlaceNote == Nothing then "Add note" else "Tap to place") ]
                , button
                    [ onClick ToggleMode
                    , style "padding" "8px 16px"
                    , style "background-color" "#334155"
                    , style "color" "#e2e8f0"
                    , style "border" "none"
                    , style "border-radius" "6px"
                    ]
                    [ text (if model.mode == MoveMode then "Switch to delete" else "Switch to move") ]
                , button
                    [ onClick CopyTextFromSelectedPlaceNote
                    , style "padding" "8px 16px"
                    , style "background-color" "#475569"
                    , style "color" "#e2e8f0"
                    , style "border" "none"
                    , style "border-radius" "6px"
                    ]
                    [ text "Load selected text" ]
                , div
                    [ style "margin-left" "auto"
                    , style "font-weight" "600"
                    ]
                    [ text ("Mode: " ++ modeLabel) ]
                ]
            , div
                [ style "padding" "0 12px 12px 12px"
                , style "font-size" "14px"
                , style "color" statusColor
                ]
                [ text statusMessage ]
            , div workspaceAttributes (List.map (\placeNote -> placeNoteView model placeNote) model.placeNotes)
            ]
            ++
            [ textarea
                [ onInput UpdateModelFromJson
                , value model.jsonTextArea
                , style "width" "100%"
                , style "min-height" "180px"
                , style "background-color" "#0f172a"
                , style "color" "#f8fafc"
                , style "border" "1px solid #1f2937"
                , style "border-radius" "0"
                , style "padding" "12px"
                , style "font-family" "JetBrains Mono, ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace"
                , style "font-size" "13px"
                , style "box-sizing" "border-box"
                ]
                []
            ])

placeNoteView : Model -> PlaceNote -> Html Msg
placeNoteView model placeNote =
    let
        -- Calculate position on-the-fly if this note is being dragged (O(1) per render)
        (displayX, displayY) =
            case model.dragging of
                Just (DraggingPlaceNote { draggedPlaceNoteId, offsetX, offsetY, currentMouseX, currentMouseY }) ->
                    if placeNote.id == draggedPlaceNoteId then
                        (currentMouseX - offsetX, currentMouseY - offsetY)
                    else
                        (placeNote.x, placeNote.y)
                _ ->
                    (placeNote.x, placeNote.y)

        noteBorder =
            if Just placeNote.id == model.selectedPlaceNoteId then
                "2px solid #f87171"
            else
                "1px solid rgba(148, 163, 184, 0.25)"
    in
    div
        [ style "position" "absolute"
        , style "left" (String.fromInt (displayX + model.cameraX) ++ "px")
        , style "top" (String.fromInt (displayY + model.cameraY) ++ "px")
        , style "width" (String.fromInt placeNote.width ++ "px")
        , style "height" (String.fromInt placeNote.height ++ "px")
        , style "background" "linear-gradient(135deg, #2563eb, #7c3aed)"
        , style "border-radius" "12px"
        , style "color" "#f8fafc"
        , style "padding" "12px"
        , style "box-sizing" "border-box"
        , style "font-weight" "600"
        , style "line-height" "1.3"
        , style "letter-spacing" "0.01em"
        , style "border" noteBorder
        , style "box-shadow" "0 20px 45px rgba(15, 23, 42, 0.45)"
        , style "user-select" "none"
        , style "white-space" "pre-wrap"
        , style "overflow-wrap" "anywhere"
        , Html.Attributes.attribute "data-id" (String.fromInt placeNote.id)
        ]
        [ text placeNote.text ]

mouseMoveDecoder : Json.Decode.Decoder Msg
mouseMoveDecoder =
    Json.Decode.map2 (\clientX clientY -> MouseMove (clientX, clientY))
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)

mousePositionDecoder : Json.Decode.Decoder Msg
mousePositionDecoder =
    Json.Decode.map2 (\x y -> MouseDown (x, y))
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)

-- Touch event decoders for mobile support
touchMoveDecoder : Json.Decode.Decoder Msg
touchMoveDecoder =
    Json.Decode.at ["touches", "0"] touchCoordinatesDecoder
        |> Json.Decode.map (\(x, y) -> MouseMove (x, y))

touchStartDecoder : Json.Decode.Decoder Msg
touchStartDecoder =
    Json.Decode.at ["touches", "0"] touchCoordinatesDecoder
        |> Json.Decode.map (\(x, y) -> MouseDown (x, y))

touchCoordinatesDecoder : Json.Decode.Decoder (Int, Int)
touchCoordinatesDecoder =
    Json.Decode.map2 (\x y -> (x, y))
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)

-- Decoder to ignore the dragstart event's default action
preventDragStart : Html.Attribute Msg
preventDragStart =
    preventDefaultOn "dragstart" (Json.Decode.succeed (NoOp, True))

-- Convert the Model to a JSON string
serializeModel : Model -> String
serializeModel model =
    Json.Encode.encode 2 <| Json.Encode.object
        [ ( "placeNotes", encodePlaceNotes model.placeNotes )
        , ( "nextPlaceNoteId", Json.Encode.int model.nextPlaceNoteId )
        , ( "cameraX", Json.Encode.int model.cameraX )
        , ( "cameraY", Json.Encode.int model.cameraY )
        , ( "mode", Json.Encode.string <| case model.mode of
                                    MoveMode -> "MoveMode"
                                    DeletionMode -> "DeletionMode" )
        , ( "inputText", Json.Encode.string model.inputText )
        , ( "selectedPlaceNoteId", case model.selectedPlaceNoteId of
            Just id -> Json.Encode.int id
            Nothing -> Json.Encode.null )
        ]

encodePlaceNotes : List PlaceNote -> Json.Encode.Value
encodePlaceNotes notes =
    let
        encodeNote : PlaceNote -> Json.Encode.Value
        encodeNote note =
            Json.Encode.object
                [ ("id", Json.Encode.int note.id)
                , ("x", Json.Encode.int note.x)
                , ("y", Json.Encode.int note.y)
                , ("width", Json.Encode.int note.width)
                , ("height", Json.Encode.int note.height)
                , ("text", Json.Encode.string note.text)
                ]
    in
    Json.Encode.list encodeNote notes

-- Convert a JSON string to the Model
deserializeModel : String -> Result String Model
deserializeModel jsonString =
    case Json.Decode.decodeString modelDecoder jsonString of
        Ok model -> Ok model
        Err error -> Err ("Failed to decode JSON: " ++ Json.Decode.errorToString error)

-- For Debug
-- deserializeModel : String -> Result String Model
-- deserializeModel jsonString =
    -- case Json.Decode.decodeString modelDecoder jsonString of
        -- Ok model -> Ok model
        -- Err error -> Err (error |> Debug.toString)

modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    map10 Model
        (Json.Decode.field "placeNotes" <| Json.Decode.list placeNoteDecoder)
        (Json.Decode.field "nextPlaceNoteId" Json.Decode.int)
        (Json.Decode.succeed Nothing) -- Default value for `dragging`
        (Json.Decode.field "cameraX" Json.Decode.int)
        (Json.Decode.field "cameraY" Json.Decode.int)
        (Json.Decode.field "mode" <| Json.Decode.map (\m -> if m == "MoveMode" then MoveMode else DeletionMode) Json.Decode.string)
        (Json.Decode.field "inputText" Json.Decode.string)
        (Json.Decode.maybe <| Json.Decode.field "selectedPlaceNoteId" Json.Decode.int)
        (Json.Decode.succeed "") -- Default value for `jsonTextArea` (not serialized, managed separately)
        (Json.Decode.succeed Nothing) -- Default for `pendingNewPlaceNote`

placeNoteDecoder : Json.Decode.Decoder PlaceNote
placeNoteDecoder =
    map6 PlaceNote
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "text" Json.Decode.string)

map6 : (a -> b -> c -> d -> e -> f -> result) 
     -> Json.Decode.Decoder a 
     -> Json.Decode.Decoder b 
     -> Json.Decode.Decoder c 
     -> Json.Decode.Decoder d 
     -> Json.Decode.Decoder e 
     -> Json.Decode.Decoder f 
     -> Json.Decode.Decoder result
map6 constructor decA decB decC decD decE decF =
    Json.Decode.map5 constructor decA decB decC decD decE
        |> Json.Decode.andThen (\valueSoFar ->
            Json.Decode.map (\fValue -> valueSoFar fValue) decF
           )

map9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> result)
     -> Json.Decode.Decoder a
     -> Json.Decode.Decoder b
     -> Json.Decode.Decoder c
     -> Json.Decode.Decoder d
     -> Json.Decode.Decoder e
     -> Json.Decode.Decoder f
     -> Json.Decode.Decoder g
     -> Json.Decode.Decoder h
     -> Json.Decode.Decoder i
     -> Json.Decode.Decoder result
map9 constructor decA decB decC decD decE decF decG decH decI =
    Json.Decode.map5 (\a b c d e -> constructor a b c d e) decA decB decC decD decE
        |> Json.Decode.andThen (\valueSoFar5 ->
            Json.Decode.map4 (\f g h i -> valueSoFar5 f g h i) decF decG decH decI
        )

map10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> result)
      -> Json.Decode.Decoder a
      -> Json.Decode.Decoder b
      -> Json.Decode.Decoder c
      -> Json.Decode.Decoder d
      -> Json.Decode.Decoder e
      -> Json.Decode.Decoder f
      -> Json.Decode.Decoder g
      -> Json.Decode.Decoder h
      -> Json.Decode.Decoder i
      -> Json.Decode.Decoder j
      -> Json.Decode.Decoder result
map10 constructor decA decB decC decD decE decF decG decH decI decJ =
    Json.Decode.map5 (\a b c d e -> constructor a b c d e) decA decB decC decD decE
        |> Json.Decode.andThen (\valueSoFar5 ->
            Json.Decode.map5 (\f g h i j -> valueSoFar5 f g h i j) decF decG decH decI decJ
        )

