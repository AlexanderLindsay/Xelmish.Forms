[<RequireQualifiedAccess>]
module Xelmish.Program

open Elmish
open GameLoop
open Model

type GameInfo = {
    Exit: unit -> unit
    GetAssets: unit -> LoadedAssets option
}

/// Entry point to run an Elmish application using Xelmish.
/// Expects a GameConfig variable with basic game loop setup config (e.g. resolution, assets)
let runGameLoop config (program: Program<_, _, _, LoadedAssets -> Viewable list>) =
    use loop = new GameLoop (config)
    let gameInfo = {
        Exit = fun () -> loop.Exit();
        GetAssets = fun () -> loop.GetLoadedAssets;
    }
    let setState model dispatch =
        loop.SetView <| (Program.view program) model dispatch
    program
    |> Program.withSetState setState 
    |> Program.runWith gameInfo
    loop.Run ()

/// Alternative entry point to Xelmish from Elmish.
/// Accepts variables to configure common config properties for the game loop
let runSimpleGameLoop assetsToLoad (windowWidth, windowHeight) clearColour (program: Program<_, _, _, LoadedAssets -> Viewable list>) =
    let config = {
        resolution = Windowed (windowWidth, windowHeight)
        clearColour = Some clearColour
        mouseVisible = true
        assetsToLoad = assetsToLoad }
    runGameLoop config program
