module Xelmish.Forms.Fields.NumberField

open Xelmish.Model
open Xelmish.Viewables

open System
open System.Text.RegularExpressions

open Field

type Model = Field.Model<float>

type OutMessage = Field.OutMessage<float>

type Message
    = Increment
    | Decrement
    | FieldMsg of Field.Message

let tryParseFloat str =
    try
        float str
        |> Valid
    with
        | :? FormatException as fe -> Invalid (str, fe.Message)

let init gameInfo width font id label value =
    initField gameInfo tryParseFloat (fun v -> sprintf "%.0f" v) id width font label value

let update gameInfo model msg =
    match msg with
    | Increment ->
        match model.Value with
        | Valid i ->
            let n = i + 1.
            let r = sprintf "%g" n
            setValue gameInfo model r model.Cursor
        | Invalid _ -> model, NoOutMessage
        | NoValue -> model, NoOutMessage
    | Decrement ->
        match model.Value with
        | Valid i ->
            let n = i - 1.
            let r = sprintf "%g" n
            setValue gameInfo model r model.Cursor
        | Invalid _ -> model, NoOutMessage
        | NoValue -> model, NoOutMessage
    | FieldMsg fmsg ->
        Field.update gameInfo model fmsg

let private isdigit = Regex("[\d\.-]+")

let private isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

let drawButton texture (width, height) (x, y) =
    OnDraw (fun loadedAssets inputs spriteBatch -> 
        
        let mouseOver = isInside x y width height inputs.mouseState.X inputs.mouseState.Y

        let backgroundColour = 
            match mouseOver with
            | true -> Colour.Blue
            | false -> Colour.LightGray

        spriteBatch.Draw(loadedAssets.textures.[texture], rect x y width height, backgroundColour)
    )

let view (x,y) fieldHeight isFocused model dispatch assets =

    let arrowButtonWidth = 25
    let arrowButtonSize = (arrowButtonWidth, fieldHeight / 2)

    let ((fpx, fpy), fieldViewables) = buildFieldView (x,y) fieldHeight isFocused model (FieldMsg >> dispatch) assets
    let upArrowPosition = (fpx + model.Window.Width - arrowButtonWidth, fpy)
    let downArrowPosition = (fpx + model.Window.Width - arrowButtonWidth, fpy + fieldHeight / 2)

    [
        yield! fieldViewables
        drawButton "up-triangle" arrowButtonSize upArrowPosition
        drawButton "down-triangle" arrowButtonSize downArrowPosition
        onclick (fun () -> 
            dispatch (Increment)
        ) arrowButtonSize upArrowPosition
        onclick (fun () -> 
            dispatch (Decrement)
        ) arrowButtonSize downArrowPosition
        yield! buildEventHandlers isdigit isFocused (FieldMsg >> dispatch)
    ]