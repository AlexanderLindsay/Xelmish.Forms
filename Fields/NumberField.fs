module Xelmish.Form.Fields.NumberField

open Xelmish.Model
open Xelmish.Viewables
open Microsoft.Xna.Framework

open FieldValueState
open System

type Model = {
    Id: string
    Value: FieldValueState<float>
    Raw: string
    Cursor: int
    Label: string
}

type OutMessage
    = NoOutMessage
    | Focus of string
    | ChangeValue of float option

type Message
    = AddValue of string
    | Delete
    | Backspace
    | ClearValue
    | Increment
    | Decrement
    | CursorLeft
    | CursorRight
    | Focus

let init id label value =
    {
        Id = id;
        Value = 
            value
            |> Option.map (fun v -> Valid v)
            |> Option.defaultValue NoValue;
        Raw =
            value
            |> Option.map (fun v -> sprintf "%.0f" v)
            |> Option.defaultValue "";
        Cursor = 0;
        Label = label;
    }

let tryParseFloat str =
    try
        float str
        |> Valid
    with
        | :? FormatException as fe -> Invalid (str, fe.Message)

let setCursor str index =
    Math.Max (0, Math.Min (String.length str, index))

let setValue model str cursor =
    let value' = tryParseFloat str
    let cmd =
        match model.Value = value' with
        | true -> NoOutMessage
        | false ->
            match value' with
            | Valid f -> ChangeValue <| Some f
            | Invalid (_,_) -> NoOutMessage
            | NoValue -> ChangeValue None
    { model with 
        Raw = str; 
        Value = value';
        Cursor = setCursor str cursor;
    }, cmd

let addTo model str =
    let raw' = model.Raw.Insert(model.Cursor, str)
    setValue model raw' (model.Cursor + 1)

let backspace model =
    match model.Cursor with
    | 0 -> model, NoOutMessage
    | c ->
        let cursor = setCursor model.Raw c - 1
        let raw' = model.Raw.Remove(cursor, 1)
        setValue model raw' cursor

let deleteValue model =
    match model.Cursor with
    | mc when mc = (String.length model.Raw) -> model, NoOutMessage
    | _ ->
        let raw' = model.Raw.Remove(model.Cursor, 1)
        setValue model raw' model.Cursor

let update model msg =
    match msg with
    | AddValue str ->
        addTo model str
    | Backspace ->
        backspace model
    | Delete ->
        deleteValue model
    | ClearValue ->
        { model with Value = NoValue; Raw = ""; Cursor = 0; }, ChangeValue None
    | Increment ->
        match model.Value with
        | Valid i ->
            let n = i + 1.
            let r = sprintf "%.0f" n
            setValue model r model.Cursor
        | Invalid _ -> model, NoOutMessage
        | NoValue -> model, NoOutMessage
    | Decrement ->
        match model.Value with
        | Valid i ->
            let n = i - 1.
            let r = sprintf "%.0f" n
            setValue model r model.Cursor
        | Invalid _ -> model, NoOutMessage
        | NoValue -> model, NoOutMessage
    | CursorLeft ->
        { model with Cursor = setCursor model.Raw (model.Cursor - 1) }, NoOutMessage
    | CursorRight ->
        { model with Cursor = setCursor model.Raw (model.Cursor + 1) }, NoOutMessage
    | Focus ->
        model, OutMessage.Focus model.Id

let buildEventHandlers isFocused dispatch =
    match isFocused with
    | true ->
        [
            onkeydown Keys.Right (fun _ -> dispatch (CursorRight))
            onkeydown Keys.Left (fun _ -> dispatch (CursorLeft))
            onkeydown Keys.NumPad0 (fun _ -> dispatch (AddValue "0"))
            onkeydown Keys.D0 (fun _ -> dispatch (AddValue "0"))
            onkeydown Keys.NumPad1 (fun _ -> dispatch (AddValue "1"))
            onkeydown Keys.D1 (fun _ -> dispatch (AddValue "1"))
            onkeydown Keys.NumPad2 (fun _ -> dispatch (AddValue "2"))
            onkeydown Keys.D2 (fun _ -> dispatch (AddValue "2"))
            onkeydown Keys.NumPad3 (fun _ -> dispatch (AddValue "3"))
            onkeydown Keys.D3 (fun _ -> dispatch (AddValue "3"))
            onkeydown Keys.NumPad4 (fun _ -> dispatch (AddValue "4"))
            onkeydown Keys.D4 (fun _ -> dispatch (AddValue "4"))
            onkeydown Keys.NumPad5 (fun _ -> dispatch (AddValue "5"))
            onkeydown Keys.D5 (fun _ -> dispatch (AddValue "5"))
            onkeydown Keys.NumPad6 (fun _ -> dispatch (AddValue "6"))
            onkeydown Keys.D6 (fun _ -> dispatch (AddValue "6"))
            onkeydown Keys.NumPad7 (fun _ -> dispatch (AddValue "7"))
            onkeydown Keys.D7 (fun _ -> dispatch (AddValue "7"))
            onkeydown Keys.NumPad8 (fun _ -> dispatch (AddValue "8"))
            onkeydown Keys.D8 (fun _ -> dispatch (AddValue "8"))
            onkeydown Keys.NumPad9 (fun _ -> dispatch (AddValue "9"))
            onkeydown Keys.D9 (fun _ -> dispatch (AddValue "9"))
            onkeydown Keys.OemPeriod (fun _ -> dispatch (AddValue "."))
            onkeydown Keys.Decimal (fun _ -> dispatch (AddValue "."))
            onkeydown Keys.Back (fun _ -> dispatch (Backspace))
            onkeydown Keys.Delete (fun _ -> dispatch (Delete))
        ]
    | false -> []

let getTextSize (assets: LoadedAssets) fontName (text: string) =
    assets.fonts
    |> Map.tryFind fontName
    |> Option.map (fun font ->
        font.MeasureString (text)
    )
    |> Option.defaultValue (Vector2 (0f, 0f))

let drawCursor (x,y) height font (fontSize: float) (text: string) cursor assets =
    let measured = getTextSize assets font (text.Substring (0, cursor))
    let xPosition = x + int measured.X

    OnDraw (fun loadedAssets _ spriteBatch -> 
        spriteBatch.Draw(loadedAssets.whiteTexture, rect xPosition y 2 height, Color.Black)
    )

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
    let gap = 5
    let labelSize = getTextSize assets "basic" model.Label
    let fieldPosition = (x, y + int labelSize.Y + gap)

    let arrowButtonWidth = 25
    let upArrowPosition = (x + width - arrowButtonWidth, y + int labelSize.Y + gap)
    let arrowButtonSize = (arrowButtonWidth, fieldHeight / 2)
    let downArrowPosition = (x + width - arrowButtonWidth, y + int labelSize.Y + gap + fieldHeight / 2)

    [
        text "basic" 18. Colour.Black (0.,0.) model.Label (x,y)
        colour Colour.BlanchedAlmond (width - arrowButtonWidth, fieldHeight) fieldPosition
        text "basic" 18. Colour.Black (0.,0.) model.Raw fieldPosition
        yield! (if isFocused then [drawCursor fieldPosition 16 "basic" 18. model.Raw model.Cursor assets] else [])
        drawButton "up-triangle" arrowButtonSize upArrowPosition
        drawButton "down-triangle" arrowButtonSize downArrowPosition
        onclick (fun () -> 
            dispatch (Focus)
        ) (width, int labelSize.Y + gap + fieldHeight) (x, y)
        onclick (fun () -> 
            dispatch (Increment)
        ) arrowButtonSize upArrowPosition
        onclick (fun () -> 
            dispatch (Decrement)
        ) arrowButtonSize downArrowPosition
        yield! buildEventHandlers isFocused dispatch
    ]