module Xelmish.Form.Fields.NumberField

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

let init id label value =
    initField tryParseFloat (fun v -> sprintf "%.0f" v) id label value

let update model msg =
    match msg with
    | Increment ->
        match model.Value with
        | Valid i ->
            let n = i + 1.
            let r = sprintf "%g" n
            setValue model r model.Cursor
        | Invalid _ -> model, NoOutMessage
        | NoValue -> model, NoOutMessage
    | Decrement ->
        match model.Value with
        | Valid i ->
            let n = i - 1.
            let r = sprintf "%g" n
            setValue model r model.Cursor
        | Invalid _ -> model, NoOutMessage
        | NoValue -> model, NoOutMessage
    | FieldMsg fmsg ->
        Field.update model fmsg

let private isdigit = Regex("[\d\.-]+")

let buildEventHandlers isFocused dispatch =
    match isFocused with
    | true ->
        [
            onupdate (fun inputs ->
                match inputs.typedValues with
                | v when isdigit.IsMatch v -> dispatch (AddValue v)
                | _ -> ()
            )
            onkeydown Keys.Right (fun _ -> dispatch (CursorRight))
            onkeydown Keys.Left (fun _ -> dispatch (CursorLeft))
            onkeydown Keys.Back (fun _ -> dispatch (Backspace))
            onkeydown Keys.Delete (fun _ -> dispatch (Delete))
            onkeydown Keys.Home (fun _ -> dispatch (CursorStart))
            onkeydown Keys.End (fun _ -> dispatch (CursorEnd))
        ]
    | false -> []

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

let view (x,y) width fieldHeight isFocused model dispatch assets =

    let arrowButtonWidth = 25
    let arrowButtonSize = (arrowButtonWidth, fieldHeight / 2)

    let ((fpx, fpy), fieldViewables) = buildFieldView (x,y) (width - arrowButtonWidth) fieldHeight isFocused model (FieldMsg >> dispatch) assets
    let upArrowPosition = (fpx + width - arrowButtonWidth, fpy)
    let downArrowPosition = (fpx + width - arrowButtonWidth, fpy + fieldHeight / 2)

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
        yield! buildEventHandlers isFocused (FieldMsg >> dispatch)
    ]