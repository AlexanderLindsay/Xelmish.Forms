module Xelmish.Form.Fields.Field

open System
open Microsoft.Xna.Framework
open Xelmish.Model
open Xelmish.Viewables

type FieldValueState<'T>
    = Valid of 'T
    | Invalid of string*string
    | NoValue

type Model<'T> = {
    Id: string
    Label: string
    Value: FieldValueState<'T>
    Cursor: int
    Raw: string
    Parse: string -> FieldValueState<'T>
    ValueToString: 'T -> string
}

type OutMessage<'T>
    = NoOutMessage
    | Focus of string
    | ChangeValue of 'T option

type Message
    = AddValue of string
    | Delete
    | Backspace
    | ClearValue
    | CursorLeft
    | CursorRight
    | CursorStart
    | CursorEnd
    | Focus

let initField parse toString id label value =
    {
        Id = id;
        Value = 
            value
            |> Option.map (fun v -> Valid v)
            |> Option.defaultValue NoValue;
        Raw =
            value
            |> Option.map (fun v -> toString v)
            |> Option.defaultValue "";
        Cursor = 0;
        Label = label;
        Parse = parse;
        ValueToString = toString;
    }

let setCursor str index =
    Math.Max (0, Math.Min (String.length str, index))

let setValue model str cursor =
    let value' = model.Parse str
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
    | CursorLeft ->
        { model with Cursor = setCursor model.Raw (model.Cursor - 1) }, NoOutMessage
    | CursorRight ->
        { model with Cursor = setCursor model.Raw (model.Cursor + 1) }, NoOutMessage
    | CursorStart ->
        { model with Cursor = setCursor model.Raw 0 }, NoOutMessage
    | CursorEnd ->
        { model with Cursor = setCursor model.Raw model.Raw.Length }, NoOutMessage
    | Focus ->
        model, OutMessage.Focus model.Id

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

let buildFieldView (x,y) width fieldHeight isFocused model dispatch assets =
    let gap = 5
    let labelSize = getTextSize assets "basic" model.Label
    let fieldPosition = (x, y + int labelSize.Y + gap)

    (fieldPosition, [
        text "basic" 18. Colour.Black (0.,0.) model.Label (x,y)
        colour Colour.BlanchedAlmond (width, fieldHeight) fieldPosition
        text "basic" 18. Colour.Black (0.,0.) model.Raw fieldPosition
        yield! (if isFocused then [drawCursor fieldPosition 16 "basic" 18. model.Raw model.Cursor assets] else [])
        onclick (fun () -> 
            dispatch (Focus)
        ) (width, int labelSize.Y + gap + fieldHeight) (x, y)
    ])