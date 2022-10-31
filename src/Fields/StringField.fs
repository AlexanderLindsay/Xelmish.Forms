module Xelmish.Forms.Fields.StringField

open System.Text.RegularExpressions

open Field

type Model = Field.Model<string>

type OutMessage = Field.OutMessage<string>

type Message = Field.Message

let init gameInfo width font  id label value =
    initField gameInfo (fun s -> Valid s) (fun s -> s) id width font label value

let update = Field.update

let private ischar = Regex("[\w@./#&+-]+")

let view (x,y) fieldHeight isFocused model dispatch assets =

    let (_, fieldViewables) = buildFieldView (x,y) fieldHeight isFocused model (dispatch) assets

    [
        yield! fieldViewables
        yield! buildEventHandlers ischar isFocused (dispatch)
    ]