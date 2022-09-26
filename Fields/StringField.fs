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

let view (x,y) width fieldHeight isFocused model dispatch assets =

    let (_, fieldViewables) = buildFieldView (x,y) width fieldHeight isFocused model (dispatch) assets

    [
        yield! fieldViewables
        yield! buildEventHandlers ischar isFocused (dispatch)
    ]