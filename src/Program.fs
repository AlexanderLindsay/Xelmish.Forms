open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Xelmish.Forms
open Xelmish.Forms.Fields

type Model = {
    Form: Form.Model;
}

let init gameInfo =
    {
        Form = Form.init gameInfo [
            Form.NumField (0.,NumberField.init gameInfo 100 "basic" "test-number-field" "Number" (Some 0.) (Some 0.))
            Form.StrField ("",StringField.init gameInfo 100 "basic" "test-string-field" "String" (Some "") None)
        ]
    }, Cmd.none

type Message 
    = FormMessage of Form.Message

let update message model =
    match message with
    | FormMessage fm ->
        let f = Form.update fm model.Form
        { model with Form = f }, Cmd.none

let view model dispatch assets =
    let numField =
        model.Form.Fields
        |> List.find (fun field ->
            match field with
            | Form.NumField _ -> true
            | _ -> false
        )

    let numValue =
        match numField with
        | Form.NumField (value,_) -> value
        | _ -> 0.

    let strField =
        model.Form.Fields
        |> List.find (fun field ->
            match field with
            | Form.StrField _ -> true
            | _ -> false
        )
    
    let strValue =
        match strField with
        | Form.StrField (value,_) -> value
        | _ -> ""

    [
        colour Colour.White (200, 500) (25, 25)
        colour Colour.White (200, 500) (250, 25)
        text "basic" 18. Colour.Black (0., 0.) (sprintf "Number Value: %.2f" numValue) (250, 25)
        text "basic" 18. Colour.Black (0., 0.) (sprintf "String Value: %s" strValue) (250, 100)
        yield! Form.view (50,25) 20 75 model.Form (FormMessage >> dispatch) assets
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