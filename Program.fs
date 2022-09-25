open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Xelmish.Form
open Xelmish.Form.Fields

type Model = {
    Num: float;
    Focus: FocusTracker.Model<string>
    NumField: NumberField.Model
}

let init () =
    {
        Num = 0.
        Focus = FocusTracker.init ()
        NumField = NumberField.init "test-number-field" "Number" <| Some 0.
    }, Cmd.none

type Message 
    = FocusMessage of FocusTracker.Message<string>
    | NumField of NumberField.Message

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

let view model dispatch assets =
    [
        colour Colour.White (200, 500) (25, 25)
        text "basic" 18. Colour.Black (0., 0.) (sprintf "Value: %.2f" model.Num) (50, 50)
        yield! NumberField.view (50, 100) 100 20 (FocusTracker.isFocused model.Focus model.NumField.Id) model.NumField (NumField >> dispatch) assets
    ]

[<EntryPoint>]
let main _ =
    let config: GameConfig = {
        clearColour = Some Colour.Black
        resolution = Windowed (1100, 600)
        assetsToLoad = [
            PipelineFont ("basic", "./content/basicFont")
        ]
        mouseVisible = true
    }
    Program.mkProgram init update view
    |> Xelmish.Program.runGameLoop config
    0