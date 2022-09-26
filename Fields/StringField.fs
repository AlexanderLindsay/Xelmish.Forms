module Xelmish.Form.Fields.StringField

open Xelmish.Model
open Xelmish.Viewables

open System.Text.RegularExpressions

open Field

type Model = Field.Model<string>

type OutMessage = Field.OutMessage<string>

type Message = Field.Message

let init id label value =
    initField (fun s -> Valid s) (fun s -> s) id label value

let update = Field.update

let private ischar = Regex(".+")

let buildEventHandlers isFocused dispatch =
    match isFocused with
    | true ->
        [
            onupdate (fun inputs ->
                match inputs.typedValues with
                | v when ischar.IsMatch v -> dispatch (AddValue v)
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

let view (x,y) width fieldHeight isFocused model dispatch assets =

    let (_, fieldViewables) = buildFieldView (x,y) width fieldHeight isFocused model (dispatch) assets

    [
        yield! fieldViewables
        yield! buildEventHandlers isFocused (dispatch)
    ]