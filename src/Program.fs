open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Xelmish.Forms
open Xelmish.Forms.Fields

type Model = {
    Num: float;
    Str: string;
    Focus: FocusTracker.Model<string>
    NumField: NumberField.Model
    StrField: StringField.Model
}

let init () =
    {
        Num = 0.
        Str = ""
        Focus = FocusTracker.init ()
        NumField = NumberField.init "test-number-field" "Number" <| Some 0.
        StrField = StringField.init "test-string-field" "String" <| Some ""
    }, Cmd.none

type Message 
    = FocusMessage of FocusTracker.Message<string>
    | NumField of NumberField.Message
    | StrField of StringField.Message

let update message model =
    match message with
    | FocusMessage fmsg ->
        { model with Focus = FocusTracker.update model.Focus fmsg }, Cmd.none
    | NumField nmsg ->
        let (fieldModel, fieldCmd) = NumberField.update model.NumField nmsg
        let model' =
            match fieldCmd with
            | NumberField.OutMessage.NoOutMessage -> model
            | NumberField.OutMessage.Focus id ->
                { model with Focus = FocusTracker.changeFocus model.Focus <| Some id }
            | NumberField.OutMessage.ChangeValue fo ->
                let n' =
                    fo
                    |> Option.defaultValue model.Num
                { model with Num = n' }
        { model' with NumField = fieldModel }, Cmd.none
    | StrField smsg ->
        let (fieldModel, fieldCmd) = StringField.update model.StrField smsg
        let model' =
            match fieldCmd with
            | StringField.OutMessage.NoOutMessage -> model
            | StringField.OutMessage.Focus id ->
                { model with Focus = FocusTracker.changeFocus model.Focus <| Some id }
            | StringField.OutMessage.ChangeValue fo ->
                let s' =
                    fo
                    |> Option.defaultValue model.Str
                { model with Str = s' }
        { model' with StrField = fieldModel }, Cmd.none

let view model dispatch assets =
    [
        colour Colour.White (200, 500) (25, 25)
        colour Colour.White (200, 500) (250, 25)
        text "basic" 18. Colour.Black (0., 0.) (sprintf "Number Value: %.2f" model.Num) (250, 25)
        text "basic" 18. Colour.Black (0., 0.) (sprintf "String Value: %s" model.Str) (250, 100)
        yield! NumberField.view (50, 25) 100 20 (FocusTracker.isFocused model.Focus model.NumField.Id) model.NumField (NumField >> dispatch) assets
        yield! StringField.view (50, 100) 100 20 (FocusTracker.isFocused model.Focus model.StrField.Id) model.StrField (StrField >> dispatch) assets
    ]

[<EntryPoint>]
let main _ =
    let config: GameConfig = {
        clearColour = Some Colour.Black
        resolution = Windowed (1100, 600)
        assetsToLoad = [
            PipelineFont ("basic", "./content/basicFont")
            PipelineTexture ("up-triangle", "./content/up_texture")
            PipelineTexture ("down-triangle", "./content/down_texture")
        ]
        mouseVisible = true
    }
    Program.mkProgram init update view
    |> Xelmish.Program.runGameLoop config
    0