module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, textarea, input)
import Html.Attributes exposing (style, value, type_)
import Html.Events exposing (onClick, onMouseDown, on, preventDefaultOn, onInput, onBlur, onFocus)
import Json.Decode
import Json.Encode
import Tuple exposing (pair)

-- MAIN
main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
    { placeNotes : List PlaceNote -- List of PlaceNotes
    , nextPlaceNoteId : Int
    , dragging : Maybe DraggingState
    , cameraX : Int
    , cameraY : Int
    , mode : Mode
    , inputText : String -- Text content of the PlaceNote
    , allowDrag : Bool
    , selectedPlaceNoteId : Maybe Int -- Currently selected PlaceNote
    }

type Mode
    = MoveMode
    | DeletionMode

type DraggingState
    = DraggingPlaceNote { draggedPlaceNoteId : Int, offsetX : Int, offsetY : Int } -- State when dragging a PlaceNote
    | DraggingCamera { initialX : Int, initialY : Int }

type alias PlaceNote =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , text : String -- Text displayed on the PlaceNote
    }

init : Model
init =
    { placeNotes = []
    , nextPlaceNoteId = 0
    , dragging = Nothing
    , cameraX = 0
    , cameraY = 0
    , mode = MoveMode
    , inputText = "Write a note here." -- Default text for a new PlaceNote
    , allowDrag = True
    , selectedPlaceNoteId = Nothing
    }

-- UPDATE
type Msg
    = AddPlaceNote ( Int, Int ) -- Add a new PlaceNote
    | UpdatePlaceNoteText String -- Update text of the selected PlaceNote
    | MouseDown ( Int, Int )
    | MouseMove (Int, Int)
    | MouseUp
    | ToggleMode
    | NoOp
    | PreventDrag
    | CopyTextFromSelectedPlaceNote -- Copy text from the selected PlaceNote
    | UpdateModelFromJson String

update : Msg -> Model -> Model
update msg model =
    case msg of
        PreventDrag ->
            { model | allowDrag = False }
        
        ToggleMode ->
            case model.mode of
                MoveMode ->
                    { model | mode = DeletionMode }
                DeletionMode ->
                    { model | mode = MoveMode }
        
        UpdatePlaceNoteText newText ->
            let
                updatedPlaceNotes = 
                    case model.selectedPlaceNoteId of
                        Just selectedId ->
                            List.map (\placeNote -> if placeNote.id == selectedId then { placeNote | text = newText, width = calculateTextWidth newText } else placeNote) model.placeNotes
                        Nothing ->
                            model.placeNotes
            in
            { model | inputText = newText, placeNotes = updatedPlaceNotes }
        
        AddPlaceNote (x, y) ->
            let
                textWidth = calculateTextWidth model.inputText
                newPlaceNoteId = model.nextPlaceNoteId
                newPlaceNote =
                    { id = newPlaceNoteId
                    , x = x - textWidth -- Adjust based on new width calculation
                    , y = y - 25
                    , width = textWidth
                    , height = 50 -- Keep height static or adjust similarly
                    , text = model.inputText
                    }
            in
            { model | placeNotes = newPlaceNote :: model.placeNotes
                    , nextPlaceNoteId = newPlaceNoteId + 1
                    , selectedPlaceNoteId = Just newPlaceNoteId } -- Set as selected
        
        
        MouseMove (mouseX, mouseY) ->
            case model.dragging of
                Just (DraggingPlaceNote { draggedPlaceNoteId, offsetX, offsetY}) ->
                    { model | placeNotes = List.map (updatePlaceNotePosition (mouseX, mouseY) draggedPlaceNoteId (offsetX, offsetY)) model.placeNotes }
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
            { model | dragging = Nothing, allowDrag = True }
        
        MouseDown (mouseX, mouseY) ->
            if model.allowDrag then
                case model.mode of
                    DeletionMode ->
                        let
                            -- Reverse the list to check the placeNotes from top to bottom (last drawn to first)
                            reversedPlaceNotes = List.reverse model.placeNotes
                            clickedPlaceNote = List.head (List.filter (\placeNote -> isClickedPlaceNote (mouseX - model.cameraX, mouseY - model.cameraY) placeNote) reversedPlaceNotes)
                            -- Remove the clicked placeNote from the original list
                            updatedPlaceNotes = case clickedPlaceNote of
                                Just placeNoteToBeRemoved -> List.filter (\placeNote -> placeNote.id /= placeNoteToBeRemoved.id) model.placeNotes
                                Nothing -> model.placeNotes
                        in
                        { model | placeNotes = updatedPlaceNotes }
                    MoveMode ->
                        case findClickedPlaceNote model.placeNotes (mouseX - model.cameraX, mouseY - model.cameraY) of
                            Just placeNote ->
                                { model | dragging = Just (DraggingPlaceNote { draggedPlaceNoteId = placeNote.id, offsetX = mouseX - placeNote.x, offsetY = mouseY - placeNote.y })
                                        , selectedPlaceNoteId = Just placeNote.id }
                            Nothing ->
                                { model | dragging = Just (DraggingCamera { initialX = mouseX, initialY = mouseY }) }
            else
                { model | dragging = Nothing }
        
        CopyTextFromSelectedPlaceNote ->
            case model.selectedPlaceNoteId of
                Just selectedId ->
                    let
                        selectedPlaceNoteText = List.head (List.filter (\placeNote -> placeNote.id == selectedId) model.placeNotes) |> Maybe.map (\placeNote -> placeNote.text) |> Maybe.withDefault model.inputText
                    in
                    { model | inputText = selectedPlaceNoteText }
                Nothing ->
                    model
            
        UpdateModelFromJson jsonString ->
            case Json.Decode.decodeString modelDecoder jsonString of
                Ok newModel ->
                    newModel
                Err _ ->
                    -- Handle error, could be logging or setting an error message in the model
                    model
        
        NoOp ->
            model -- Do nothing

calculateTextWidth : String -> Int
calculateTextWidth text =
    8 * String.length text -- Approximate width calculation for PlaceNote text

-- Helper function to check if a placeNote was clicked
isClickedPlaceNote : (Int, Int) -> PlaceNote -> Bool
isClickedPlaceNote (mouseX, mouseY) placeNote =
    mouseX >= placeNote.x && mouseX <= placeNote.x + placeNote.width &&
    mouseY >= placeNote.y && mouseY <= placeNote.y + placeNote.height

-- Find a placeNote that has been clicked based on the mouse position
findClickedPlaceNote : List PlaceNote -> (Int, Int) -> Maybe PlaceNote
findClickedPlaceNote placeNotes (mouseX, mouseY) =
    case placeNotes of
        [] ->
            Nothing
        placeNote :: rest ->
            if mouseX >= placeNote.x && mouseX <= placeNote.x + placeNote.width &&
               mouseY >= placeNote.y && mouseY <= placeNote.y + placeNote.height then
                Just placeNote
            else
                findClickedPlaceNote rest (mouseX, mouseY)

-- Update position of a placeNote based on the current mouse position
-- and the dragging state
updatePlaceNotePosition : (Int, Int) -> Int -> (Int, Int) -> PlaceNote -> PlaceNote
updatePlaceNotePosition (mouseX, mouseY) draggedPlaceNoteId (offsetX, offsetY) placeNote =
    if placeNote.id == draggedPlaceNoteId then
        { placeNote | x = mouseX - offsetX, y = mouseY - offsetY }
    else
        placeNote

-- Update the dragging state based on the clicked placeNote
initiateDraggingState : (Int, Int) -> PlaceNote -> DraggingState
initiateDraggingState (mouseX, mouseY) placeNote =
    DraggingPlaceNote { draggedPlaceNoteId = placeNote.id, offsetX = mouseX - placeNote.x, offsetY = mouseY - placeNote.y }

-- VIEW
view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "background-color" "black"
        , Html.Events.on "mouseup" (Json.Decode.succeed MouseUp)
        , Html.Events.on "mousemove" mouseMoveDecoder
        , Html.Events.on "mousedown" mousePositionDecoder
        , preventDragStart
        ]
        [ input [ onMouseDown PreventDrag
                , type_ "text"
                , value model.inputText
                , onInput UpdatePlaceNoteText
                ] []
        , button [ onMouseDown PreventDrag, onClick (AddPlaceNote (100 - model.cameraX, 60 - model.cameraY)) ] [ text "Add PlaceNote" ] -- Button to add a new PlaceNote
        , button [ onMouseDown PreventDrag, onClick ToggleMode ] [ text (if model.mode == MoveMode then "Switch to Deletion Mode" else "Switch to Move Mode") ]
        , button [ onMouseDown PreventDrag, onClick CopyTextFromSelectedPlaceNote ] [ text "Copy Text from PlaceNote" ] -- Button to copy text from the selected PlaceNote
        , div [] (List.map (\placeNote -> placeNoteView model placeNote) model.placeNotes)
        , textarea [ onInput UpdateModelFromJson ] [ text (serializeModel model) ]
        ]

placeNoteView : Model -> PlaceNote -> Html Msg
placeNoteView model placeNote =
    div
        [ style "position" "absolute"
        , style "left" (String.fromInt (placeNote.x + model.cameraX) ++ "px")
        , style "top" (String.fromInt (placeNote.y + model.cameraY) ++ "px")
        , style "width" (String.fromInt placeNote.width ++ "px")
        , style "height" (String.fromInt placeNote.height ++ "px")
        , style "background-color" "#007bff"
        , style "border-radius" "8px"
        , style "color" "white"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "border" (if Just placeNote.id == model.selectedPlaceNoteId then "4px solid red" else "1px solid black") -- Highlight selected placeNote
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
        , ( "allowDrag", Json.Encode.bool model.allowDrag )
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
    map9 Model
        (Json.Decode.field "placeNotes" <| Json.Decode.list placeNoteDecoder)
        (Json.Decode.field "nextPlaceNoteId" Json.Decode.int)
        (Json.Decode.succeed Nothing) -- Default value for `dragging`
        (Json.Decode.field "cameraX" Json.Decode.int)
        (Json.Decode.field "cameraY" Json.Decode.int)
        (Json.Decode.field "mode" <| Json.Decode.map (\m -> if m == "MoveMode" then MoveMode else DeletionMode) Json.Decode.string)
        (Json.Decode.field "inputText" Json.Decode.string)
        (Json.Decode.field "allowDrag" Json.Decode.bool)
        (Json.Decode.maybe <| Json.Decode.field "selectedPlaceNoteId" Json.Decode.int)

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
