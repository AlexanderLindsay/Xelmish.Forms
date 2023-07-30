module Xelmish.Forms.Fields.Field

open System
open Microsoft.Xna.Framework
open System.Text.RegularExpressions

open Xelmish.Model
open Xelmish.Viewables
open Xelmish.Forms
open System.Globalization

type FieldId = string

type FieldValueState<'T>
    = Valid of 'T
    | Invalid of string*string
    | NoValue

type CursorState
    = Visible of TimeSpan
    | Hidden of TimeSpan

type TextWindow = {
    StartIndex: int
    EndIndex: int
    Width: int
}

type Model<'T> = {
    Id: FieldId
    Font: string
    Label: string
    Value: FieldValueState<'T>
    DefaultValue: 'T option
    Window: TextWindow
    Cursor: int
    CursorVisibleFor: TimeSpan
    CursorHiddenFor: TimeSpan
    CursorState: CursorState
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

let getStringCutoff font text boundary =
    FontUtilities.getStringCutoff text (1f,1f) boundary font

let getFont (assets: LoadedAssets) fontName =
    let font = 
        assets.fonts
        |> Map.tryFind fontName
    match font with
    | Some f -> f
    | None -> raise (ArgumentException(sprintf "No font \"%s\" could be found" fontName))

let initField (gameInfo: Xelmish.Program.GameInfo) parse toString id width font label value defaultValue =
    let visibleSpan = TimeSpan(0,0,0,0,500)
    let hiddenSpan = TimeSpan(0,0,0,0,500)
    let assets = gameInfo.GetAssets()
    let rawText = 
        value
        |> Option.map (fun v -> toString v)
        |> Option.defaultValue "";
    let endIndex =
        assets
        |> Option.map (fun assets ->
            let font = getFont assets font
            getStringCutoff font rawText width
        )
        |> Option.defaultValue rawText.Length
    {
        Id = id;
        Font = font;
        Window = {
            StartIndex = 0;
            EndIndex = endIndex;
            Width = width;
        };
        Value = 
            value
            |> Option.map (fun v -> Valid v)
            |> Option.defaultValue NoValue;
        DefaultValue = defaultValue;
        Raw = rawText
        Cursor = 0;
        CursorVisibleFor = visibleSpan;
        CursorHiddenFor = hiddenSpan;
        CursorState = Visible visibleSpan;
        Label = label;
        Parse = parse;
        ValueToString = toString;
    }

let revString str =
    let si = StringInfo(str)
    let teArr = Array.init si.LengthInTextElements (fun i -> si.SubstringByTextElements(i,1))
    Array.Reverse(teArr) //in-place reversal better performance than Array.rev
    String.Join("", teArr)

let updateWindow (gameInfo: Xelmish.Program.GameInfo) font (rawValue: string) cursor currentWindow =
    match (cursor >= currentWindow.EndIndex) with
    | true ->
        let endIndex = max cursor currentWindow.EndIndex
        let reversed = revString rawValue
        let diff = reversed.Length - endIndex
        let text = reversed.Substring(diff)
        let assets = gameInfo.GetAssets()
        let start =
            assets
            |> Option.map (fun assets ->
                let font = getFont assets font
                let cutoff = getStringCutoff font text currentWindow.Width
                cutoff + 1 + diff
            )
            |> Option.defaultValue reversed.Length
        { currentWindow with StartIndex = (reversed.Length - start); EndIndex = endIndex; }
    | false ->
        let start = min cursor currentWindow.StartIndex
        let text = rawValue.Substring(start)
        let endIndex =
            match text.Length with
            | 0 -> 0
            | _ ->
                let assets = gameInfo.GetAssets()
                assets
                |> Option.map (fun assets ->
                    let font = getFont assets font
                    let cutoff = getStringCutoff font text currentWindow.Width
                    (cutoff + 1 + start)
                )
                |> Option.defaultValue rawValue.Length
        { currentWindow with StartIndex = start; EndIndex = endIndex; }

let setCursor str index =
    Math.Max (0, Math.Min (String.length str, index))

let setValue gameInfo model str cursor =
    let value' = model.Parse str
    let cmd =
        match model.Value = value' with
        | true -> NoOutMessage
        | false ->
            match value' with
            | Valid f -> ChangeValue <| Some f
            | Invalid (_,_) -> ChangeValue model.DefaultValue
            | NoValue -> ChangeValue None
    let cursor = setCursor str cursor;
    { model with 
        Raw = str; 
        Value = value';
        Cursor = cursor;
        Window = updateWindow gameInfo model.Font str cursor model.Window
        CursorState = Visible model.CursorVisibleFor;
    }, cmd

let addTo gameInfo model str =
    let raw' = model.Raw.Insert(model.Cursor, str)
    setValue gameInfo model raw' (model.Cursor + 1)

let backspace gameInfo model =
    match model.Cursor with
    | 0 -> model, NoOutMessage
    | c ->
        let cursor = setCursor model.Raw c - 1
        let raw' = model.Raw.Remove(cursor, 1)
        setValue gameInfo model raw' cursor

let deleteValue gameInfo model =
    match model.Cursor with
    | mc when mc = (String.length model.Raw) -> model, NoOutMessage
    | _ ->
        let raw' = model.Raw.Remove(model.Cursor, 1)
        setValue gameInfo model raw' model.Cursor

let updateCursorState visibleFor hiddenFor state elapsed =
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

let changeCursor gameInfo model newCursor =
    let cursor = setCursor model.Raw newCursor
    let window = updateWindow gameInfo model.Font model.Raw cursor model.Window
    { model with Cursor = cursor; Window = window }

let update gameInfo model msg =
    match msg with
    | AddValue str ->
        addTo gameInfo model str
    | Backspace ->
        backspace gameInfo model
    | Delete ->
        deleteValue gameInfo model
    | ClearValue ->
        { model with Value = NoValue; Raw = ""; Cursor = 0; Window = {model.Window with StartIndex = 0; EndIndex = 0; } }, ChangeValue None
    | CursorLeft ->
        changeCursor gameInfo model (model.Cursor - 1), NoOutMessage
    | CursorRight ->
        changeCursor gameInfo model (model.Cursor + 1), NoOutMessage
    | CursorStart ->
        changeCursor gameInfo model 0, NoOutMessage
    | CursorEnd ->
        changeCursor gameInfo model model.Raw.Length, NoOutMessage
    | UpdateCursorTime elapsed ->
        { model with CursorState = updateCursorState model.CursorVisibleFor model.CursorHiddenFor model.CursorState elapsed}, NoOutMessage
    | Focus ->
        model, OutMessage.Focus model.Id

let drawCursor (x,y) height font (fontSize: float) (text: string) cursor cursorState =
    match cursorState with
    | Visible _ ->
        let index = Math.Min(cursor, text.Length)
        let measured = FontUtilities.measureString (text.Substring (0, index)) font
        let xPosition = x + int measured.X

        [OnDraw (fun loadedAssets _ spriteBatch -> 
            spriteBatch.Draw(loadedAssets.whiteTexture, rect xPosition y 2 height, Color.Black)
        )]
    | Hidden _ -> []

let rec createWindowedString (text: string) window =
    text.Substring(window.StartIndex, window.EndIndex - window.StartIndex)

let buildFieldView (x,y) fieldHeight isFocused model dispatch assets =
    let basicFont = getFont assets model.Font
    let gap = 5
    let labelSize = FontUtilities.measureString model.Label basicFont
    let fieldPosition = (x, y + int labelSize.Y + gap)

    let str = createWindowedString model.Raw model.Window

    (fieldPosition, [
        text model.Font 18. Colour.Black (0.,0.) model.Label (x,y)
        colour Colour.BlanchedAlmond (model.Window.Width + 3, fieldHeight + 1) (fst fieldPosition - 3, snd fieldPosition - 1)
        text model.Font 18. Colour.Black (0.,0.) str fieldPosition
        yield! (if isFocused then drawCursor fieldPosition 16 basicFont 18. str (model.Cursor - model.Window.StartIndex) model.CursorState else [])
        onclick (fun () -> 
            dispatch (Focus)
        ) (model.Window.Width, int labelSize.Y + gap + fieldHeight) (x, y)
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