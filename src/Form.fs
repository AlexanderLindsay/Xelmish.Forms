module Xelmish.Forms.Form

open Xelmish.Model
open Xelmish.Viewables

open Xelmish.Forms.Fields
open Xelmish.Forms.Fields.Field

type FormField
    = NumField of float*NumberField.Model
    | StrField of string*StringField.Model

type FormFieldMessage
    = NumMessage of NumberField.Message
    | StrMessage of StringField.Message

type Model = {
    Game: Xelmish.Program.GameInfo;
    Fields: FormField list;
    Focus: FocusTracker.Model<Field.FieldId>;
    Errors: string list;
}

let init gameInfo fields =
    {
        Game = gameInfo;
        Fields = fields;
        Focus = FocusTracker.init ();
        Errors = [];
    }

type Message 
    = FocusMessage of FocusTracker.Message<string>
    | FieldMessage of Field.FieldId*FormFieldMessage
    | DeltaFocus of int

type FormFieldOutMessage
    = NoOutMessage
    | Focus of string

let toFieldId field =
    match field with
    | NumField (_,f) -> f.Id
    | StrField (_,f) -> f.Id

let matchFieldId id field =
    match field with
    | NumField (_,nf) ->
        nf.Id = id
    | StrField (_,sf) ->
        sf.Id = id

let replaceField id replacement field = 
    match field with
    | NumField (_,nf) ->
        match nf.Id with
        | x when x = id -> replacement
        | _ -> field
    | StrField (_,sf) ->
        match sf.Id with
        | x when x = id -> replacement
        | _ -> field

let update message model =
    match message with
    | FocusMessage fmsg ->
        { model with Focus = FocusTracker.update model.Focus fmsg }
    | DeltaFocus delta->
            match model.Focus.Focused with
            | Some focusedId ->
                try
                    let fieldIds =
                        model.Fields
                        |> List.map toFieldId
                    let currentIndex =
                        fieldIds
                        |> List.findIndex (fun id -> id = focusedId)
                    
                    let newIndex = currentIndex + delta
                    let newId = 
                        match newIndex with
                        | i when i >= List.length fieldIds ->
                            List.head fieldIds
                        | i when i < 0 ->
                            List.last fieldIds
                        | i ->
                            List.item i fieldIds
                    { model with Focus = FocusTracker.changeFocus model.Focus (Some newId) }
                with
                | :? System.Collections.Generic.KeyNotFoundException ->
                    { model with Errors = (sprintf "Could not field field with an id of '%s'" focusedId) :: model.Errors }
            | None ->
                model
    | FieldMessage (fieldId,fieldMessage) ->
        try
            let field =
                model.Fields
                |> List.find (matchFieldId fieldId)
            let (field', fieldCmd) =
                match (fieldMessage, field) with
                | (NumMessage numMessage, NumField (v, numModel)) ->
                    let (m,o) = NumberField.update model.Game numModel numMessage
                    let (vl, ffo) =
                        match o with
                        | ChangeValue fo ->
                            let v' =
                                fo
                                |> Option.defaultValue v
                            (v', NoOutMessage)
                        | OutMessage.Focus id ->
                            (v, Focus id)
                        | OutMessage.NoOutMessage ->
                            (v, NoOutMessage)
                    (NumField (vl,m)),ffo
                | (StrMessage strMessage, StrField (v, strModel)) ->
                    let (m,o) = StringField.update model.Game strModel strMessage
                    let (vl, ffo) =
                        match o with
                        | ChangeValue fo ->
                            let v' =
                                fo
                                |> Option.defaultValue v
                            (v', NoOutMessage)
                        | OutMessage.Focus id ->
                            (v, Focus id)
                        | OutMessage.NoOutMessage ->
                            (v, NoOutMessage)
                    (StrField (vl,m)),ffo
                | _ ->
                    (field, NoOutMessage)
            let model' = 
                match fieldCmd with
                | NoOutMessage -> model
                | Focus id ->
                    { model with Focus = FocusTracker.changeFocus model.Focus <| Some id }
            { model' with 
                Fields = 
                    model.Fields
                    |> List.map (replaceField fieldId field')
            }
        with
        | :? System.Collections.Generic.KeyNotFoundException ->
            { model with Errors = (sprintf "Could not field field with an id of '%s'" fieldId) :: model.Errors }

let layoutFields (ix,iy) fieldHeight heightSpacing focus fields dispatch assets =
    let (fields', height) = 
        (iy, fields)
        ||> List.mapFold (fun y field ->
            match field with
            | NumField (_,numField) ->
                let dp = fun m ->
                    let nm = NumMessage m
                    FieldMessage (numField.Id,nm)
                    |> dispatch
                let f = NumberField.view (ix, y) fieldHeight (FocusTracker.isFocused focus numField.Id) numField dp assets
                f, (y + heightSpacing)
            | StrField (_,strField) ->
                let dp = fun m ->
                    let sm = StrMessage m
                    FieldMessage (strField.Id,sm)
                    |> dispatch
                let f = StringField.view (ix, y) fieldHeight (FocusTracker.isFocused focus strField.Id) strField dp assets
                f, (y + heightSpacing)
        )
    let fs =
        fields'
        |> List.collect id
    (fs, height)

let viewErrors (ix,iy ) spacing errors =
    let (viewables, height) = 
        (iy, errors)
        ||> List.mapFold (fun y error ->
            let textViewable = text "basic" 18. Colour.Red (0., 0.) error (ix, y)
            (textViewable, y + spacing)
        )
    viewables

let view (x,y) fieldHeights spacing model dispatch assets =
    let (fields', fy) = layoutFields (x,y) fieldHeights spacing model.Focus model.Fields dispatch assets
    [
        yield! fields';
        yield! viewErrors (x, fy) 25 model.Errors;
        onkeydownWithoutShift Keys.Tab (fun _ -> dispatch (DeltaFocus 1));
        onkeydownWithShift Keys.Tab (fun _ -> dispatch (DeltaFocus -1));
    ]