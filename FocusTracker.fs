module Xelmish.Form.FocusTracker

type Model<'T when 'T: equality> = {
    Focused: 'T option
}

type Message<'T when 'T: equality>
    = ChangeFocus of 'T option

let init () =
    {
        Focused = None
    }

let isFocused<'T when 'T: equality> model id =
    match model.Focused with
    | Some (t: 'T) -> t = id
    | None -> false

let changeFocus model id =
    { model with Focused = id }

let update model msg =
    match msg with
    | ChangeFocus newFocus ->
        { model with Focused = newFocus }