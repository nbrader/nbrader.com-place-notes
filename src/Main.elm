module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown)
import Json.Decode as Decode
import Tuple exposing (pair)

-- MAIN
main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
    { rectangles : List Rectangle
    , nextRectangleId : Int
    , dragging : Maybe DraggingState
    }

type alias DraggingState =
    { draggedId : Int
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
    { rectangles = []
    , nextRectangleId = 0
    , dragging = Nothing
    }

-- UPDATE
type Msg
    = AddRectangle ( Int, Int )
    | MouseDown ( Int, Int ) -- Used when clicking to start dragging
    | EndDrag
    | MouseMove (Int, Int)
    | MouseUp

-- Simulated decoder for starting drag; assumes dragging starts anywhere
mousePositionDecoder : Decode.Decoder Msg
mousePositionDecoder =
    Decode.map2 (\x y -> MouseDown (x, y)) -- Changed to MouseDown for clarity
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)

mouseMoveDecoder : Decode.Decoder Msg
mouseMoveDecoder =
    Decode.map2 (\clientX clientY -> MouseMove (clientX, clientY))
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddRectangle (x, y) ->
            let
                newRect =
                    { id = model.nextRectangleId
                    , x = x - 50
                    , y = y - 25
                    , width = 100
                    , height = 50
                    }
            in
            { model | rectangles = newRect :: model.rectangles, nextRectangleId = model.nextRectangleId + 1 }
        
        EndDrag ->
            { model | dragging = Nothing }
        
        MouseMove (mouseX, mouseY) ->
            case model.dragging of
                Just dragState ->
                    let
                        updatePosition rect =
                            if rect.id == dragState.draggedId then
                                -- Apply offsets to keep the rectangle under the cursor as it was at the start of the drag
                                { rect | x = mouseX - dragState.offsetX, y = mouseY - dragState.offsetY }
                            else
                                rect
                    in
                    { model | rectangles = List.map updatePosition model.rectangles }
                Nothing ->
                    model
            
        MouseUp ->
            { model | dragging = Nothing }
        
        MouseDown (mouseX, mouseY) ->
            let
                findClickedRect rects =
                    case rects of
                        [] ->
                            Nothing
                        rect :: rest ->
                            if mouseX >= rect.x && mouseX <= rect.x + rect.width &&
                               mouseY >= rect.y && mouseY <= rect.y + rect.height then
                                Just rect
                            else
                                findClickedRect rest

                maybeClickedRect = findClickedRect model.rectangles
                newDraggingState =
                    Maybe.map (\rect -> { draggedId = rect.id, offsetX = mouseX - rect.x, offsetY = mouseY - rect.y }) maybeClickedRect
            in
            { model | dragging = newDraggingState }

-- VIEW
view : Model -> Html Msg
view model =
    div [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100vh"
        , Html.Events.on "mousemove" mouseMoveDecoder
        , Html.Events.on "mouseup" (Decode.succeed MouseUp)
        ]
        [ button [ onClick (AddRectangle (0, 0)) ] [ text "Add" ]
        , div [] (List.map rectangleView model.rectangles)
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
        ]
        []
