module Xelmish.Form.Fields.Field

open System
open Microsoft.Xna.Framework
open Xelmish.Model
open Xelmish.Viewables
open System.Text.RegularExpressions

type FieldValueState<'T>
    = Valid of 'T
    | Invalid of string*string
    | NoValue

type CusorState
    = Visible of TimeSpan
    | Hidden of TimeSpan

type Model<'T> = {
    Id: string
    Label: string
    Value: FieldValueState<'T>
    Cursor: int
    CusorVisibleFor: TimeSpan
    CusorHiddenFor: TimeSpan
    CusorState: CusorState
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
    | UpdateCursorTime of TimeSpan
    | Focus

let initField parse toString id label value =
    let visibleSpan = TimeSpan(0,0,0,0,500)
    let hiddenSpan = TimeSpan(0,0,0,0,500)
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
        CusorVisibleFor = visibleSpan;
        CusorHiddenFor = hiddenSpan;
        CusorState = Visible visibleSpan;
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

let updateCusorState visibleFor hiddenFor state elapsed =
    match state with
    | Visible remaining ->
        let remaining' = remaining - elapsed
        match remaining.TotalMilliseconds > 0 with
        | true -> Visible remaining'
        | false -> Hidden hiddenFor
    | Hidden remaining ->
        let remaining' = remaining - elapsed
        match remaining.TotalMilliseconds > 0 with
        | true -> Hidden remaining'
        | false -> Visible visibleFor

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
    | UpdateCursorTime elapsed ->
        { model with CusorState = updateCusorState model.CusorVisibleFor model.CusorHiddenFor model.CusorState elapsed}, NoOutMessage
    | Focus ->
        model, OutMessage.Focus model.Id

let getTextSize (assets: LoadedAssets) fontName (text: string) =
    assets.fonts
    |> Map.tryFind fontName
    |> Option.map (fun font ->
        font.MeasureString (text)
    )
    |> Option.defaultValue (Vector2 (0f, 0f))

let drawCursor (x,y) height font (fontSize: float) (text: string) cursor cursorState assets =
    match cursorState with
    | Visible _ ->
        let measured = getTextSize assets font (text.Substring (0, cursor))
        let xPosition = x + int measured.X

        [OnDraw (fun loadedAssets _ spriteBatch -> 
            spriteBatch.Draw(loadedAssets.whiteTexture, rect xPosition y 2 height, Color.Black)
        )]
    | Hidden _ -> []

let buildFieldView (x,y) width fieldHeight isFocused model dispatch assets =
    let gap = 5
    let labelSize = getTextSize assets "basic" model.Label
    let fieldPosition = (x, y + int labelSize.Y + gap)

    (fieldPosition, [
        text "basic" 18. Colour.Black (0.,0.) model.Label (x,y)
        colour Colour.BlanchedAlmond (width, fieldHeight) fieldPosition
        text "basic" 18. Colour.Black (0.,0.) model.Raw fieldPosition
        yield! (if isFocused then drawCursor fieldPosition 16 "basic" 18. model.Raw model.Cursor model.CusorState assets else [])
        onclick (fun () -> 
            dispatch (Focus)
        ) (width, int labelSize.Y + gap + fieldHeight) (x, y)
    ])

let buildEventHandlers (validKeyRegex: Regex) isFocused dispatch =
    match isFocused with
    | true ->
        [
            onupdate (fun inputs ->
                match inputs.typedValues with
                | v when validKeyRegex.IsMatch v -> dispatch (AddValue v)
                | _ -> ()
            )
            onupdate (fun inputs ->
                dispatch (UpdateCursorTime inputs.gameTime.ElapsedGameTime)
            )
            onkeydown Keys.Right (fun _ -> dispatch (CursorRight))
            onkeydown Keys.Left (fun _ -> dispatch (CursorLeft))
            onkeydown Keys.Back (fun _ -> dispatch (Backspace))
            onkeydown Keys.Delete (fun _ -> dispatch (Delete))
            onkeydown Keys.Home (fun _ -> dispatch (CursorStart))
            onkeydown Keys.End (fun _ -> dispatch (CursorEnd))
        ]
    | false -> []