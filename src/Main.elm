module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, on, preventDefaultOn)
import Json.Decode as Decode
import Tuple exposing (pair)

-- MAIN
main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
    { rects : List Rectangle
    , nextRectangleId : Int
    , dragging : Maybe DraggingState
    , cameraX : Int
    , cameraY : Int
    , mode : Mode
    }

type Mode
    = MoveMode
    | DeletionMode

type DraggingState
    = DraggingRectangle { draggedRectangleId : Int, offsetX : Int, offsetY : Int }
    | DraggingCamera { initialX : Int, initialY : Int }

type alias Rectangle =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }

init : Model
init =
    { rects = []
    , nextRectangleId = 0
    , dragging = Nothing
    , cameraX = 0
    , cameraY = 0
    , mode = MoveMode -- Add this line
    }

-- UPDATE
type Msg
    = AddRectangle ( Int, Int )
    | MouseDown ( Int, Int )
    | MouseMove (Int, Int)
    | MouseUp
    | ToggleMode -- Add this line
    | NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleMode ->
            case model.mode of
                MoveMode ->
                    { model | mode = DeletionMode }
                DeletionMode ->
                    { model | mode = MoveMode }
        
        AddRectangle (x, y) ->
            let
                newRectangle =
                    { id = model.nextRectangleId
                    , x = x - 50
                    , y = y - 25
                    , width = 100
                    , height = 50
                    }
            in
            { model | rects = newRectangle :: model.rects, nextRectangleId = model.nextRectangleId + 1 }
        
        MouseMove (mouseX, mouseY) ->
            case model.dragging of
                Just (DraggingRectangle { draggedRectangleId, offsetX, offsetY}) ->
                    { model | rects = List.map (updateRectanglePosition (mouseX, mouseY) draggedRectangleId (offsetX, offsetY)) model.rects }
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
            { model | dragging = Nothing }
        
        MouseDown (mouseX, mouseY) ->
            case model.mode of
                DeletionMode ->
                    let
                        -- Reverse the list to check the rectangles from top to bottom (last drawn to first)
                        reversedRects = List.reverse model.rects
                        clickedRect = List.head (List.filter (\rect -> isClickedRectangle (mouseX - model.cameraX, mouseY - model.cameraY) rect) reversedRects)
                        -- Remove the clicked rectangle from the original list
                        updatedRects = case clickedRect of
                            Just rectToBeRemoved -> List.filter (\rect -> rect.id /= rectToBeRemoved.id) model.rects
                            Nothing -> model.rects
                    in
                    { model | rects = updatedRects }
                MoveMode ->
                    let
                        maybeClickedRectangle = findClickedRectangle model.rects (mouseX - model.cameraX, mouseY - model.cameraY)
                    in
                    case maybeClickedRectangle of
                        Just rect ->
                            let
                                dragState = DraggingRectangle { draggedRectangleId = rect.id, offsetX = mouseX - rect.x, offsetY = mouseY - rect.y }
                            in
                            { model | dragging = Just dragState }
                        Nothing ->
                            { model | dragging = Just (DraggingCamera { initialX = mouseX, initialY = mouseY }) }
        
        NoOp ->
            model -- Do nothing

-- Helper function to check if a rectangle was clicked
isClickedRectangle : (Int, Int) -> Rectangle -> Bool
isClickedRectangle (mouseX, mouseY) rect =
    mouseX >= rect.x && mouseX <= rect.x + rect.width &&
    mouseY >= rect.y && mouseY <= rect.y + rect.height

-- Find a rectangle that has been clicked based on the mouse position
findClickedRectangle : List Rectangle -> (Int, Int) -> Maybe Rectangle
findClickedRectangle rects (mouseX, mouseY) =
    case rects of
        [] ->
            Nothing
        rect :: rest ->
            if mouseX >= rect.x && mouseX <= rect.x + rect.width &&
               mouseY >= rect.y && mouseY <= rect.y + rect.height then
                Just rect
            else
                findClickedRectangle rest (mouseX, mouseY)

-- Update position of a rectangle based on the current mouse position
-- and the dragging state
updateRectanglePosition : (Int, Int) -> Int -> (Int, Int) -> Rectangle -> Rectangle
updateRectanglePosition (mouseX, mouseY) draggedRectangleId (offsetX, offsetY) rect =
    if rect.id == draggedRectangleId then
        { rect | x = mouseX - offsetX, y = mouseY - offsetY }
    else
        rect

-- Update the dragging state based on the clicked rectangle
initiateDraggingState : (Int, Int) -> Rectangle -> DraggingState
initiateDraggingState (mouseX, mouseY) rect =
    DraggingRectangle { draggedRectangleId = rect.id, offsetX = mouseX - rect.x, offsetY = mouseY - rect.y }

-- VIEW
view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100vh"
        , Html.Events.on "mouseup" (Decode.succeed MouseUp) -- This ensures MouseUp is captured over the entire div
        , Html.Events.on "mousemove" mouseMoveDecoder -- Consider if this should also be more broadly captured
        , Html.Events.on "mousedown" mousePositionDecoder
        , preventDragStart
        ]
        [ button [ onClick (AddRectangle (100 - model.cameraX, 60 - model.cameraY)) ] [ text "Add" ]
        , button [ onClick ToggleMode ] [ text (if model.mode == MoveMode then "Switch to Deletion Mode" else "Switch to Move Mode") ]
        , div [] (List.map (\rect -> rectangleView model rect) model.rects)
        ]

rectangleView : Model -> Rectangle -> Html Msg
rectangleView model rect =
    div
        [ style "position" "absolute"
        , style "left" (String.fromInt (rect.x + model.cameraX) ++ "px")
        , style "top" (String.fromInt (rect.y + model.cameraY) ++ "px")
        , style "width" (String.fromInt rect.width ++ "px")
        , style "height" (String.fromInt rect.height ++ "px")
        , style "background-color" "blue"
        , Html.Attributes.attribute "data-id" (String.fromInt rect.id)
        ]
        []

mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    Decode.map2 (\clientX clientY -> MouseMove (clientX, clientY))
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)

mousePositionDecoder : Decode.Decoder Msg
mousePositionDecoder =
    Decode.map2 (\x y -> MouseDown (x, y))
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)

-- Decoder to ignore the dragstart event's default action
preventDragStart : Html.Attribute Msg
preventDragStart =
    preventDefaultOn "dragstart" (Decode.succeed (NoOp, True))
