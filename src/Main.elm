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
    }

type alias DraggingState =
    { draggedRectangleId : Int
    , offsetX : Int
    , offsetY : Int
    }

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
    }

-- UPDATE
type Msg
    = AddRectangle ( Int, Int )
    | MouseDown ( Int, Int )
    | MouseMove (Int, Int)
    | MouseUp
    | NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of
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
                Just dragState ->
                    { model | rects = List.map (updateRectanglePosition (mouseX, mouseY) dragState) model.rects }
                Nothing ->
                    model
            
        MouseUp ->
            { model | dragging = Nothing }
        
        MouseDown (mouseX, mouseY) ->
            let
                maybeClickedRectangle = findClickedRectangle model.rects (mouseX, mouseY)
            in
            case maybeClickedRectangle of
                Just rect ->
                    { model | dragging = Just (initiateDraggingState (mouseX, mouseY) rect) }
                Nothing ->
                    model
        
        NoOp ->
            model -- Do nothing

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
updateRectanglePosition : (Int, Int) -> DraggingState -> Rectangle -> Rectangle
updateRectanglePosition (mouseX, mouseY) dragState rect =
    if rect.id == dragState.draggedRectangleId then
        { rect | x = mouseX - dragState.offsetX, y = mouseY - dragState.offsetY }
    else
        rect

-- Update the dragging state based on the clicked rectangle
initiateDraggingState : (Int, Int) -> Rectangle -> DraggingState
initiateDraggingState (mouseX, mouseY) rect =
    { draggedRectangleId = rect.id, offsetX = mouseX - rect.x, offsetY = mouseY - rect.y }

-- VIEW
view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100vh"
        , Html.Events.on "mouseup" (Decode.succeed MouseUp) -- This ensures MouseUp is captured over the entire div
        , Html.Events.on "mousemove" mouseMoveDecoder -- Consider if this should also be more broadly captured
        ]
        [ button [ onClick (AddRectangle (100, 60)) ] [ text "Add" ]
        , div [] (List.map rectangleView model.rects)
        ]

rectangleView : Rectangle -> Html Msg
rectangleView rect =
    div
        [ style "position" "absolute"
        , style "left" (String.fromInt rect.x ++ "px")
        , style "top" (String.fromInt rect.y ++ "px")
        , style "width" (String.fromInt rect.width ++ "px")
        , style "height" (String.fromInt rect.height ++ "px")
        , style "background-color" "blue"
        , Html.Attributes.attribute "data-id" (String.fromInt rect.id)
        , Html.Events.on "mousedown" mousePositionDecoder
        , preventDragStart -- Prevents the default dragstart behaviour
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