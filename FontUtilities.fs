// reference: https://community.monogame.net/t/measurestring-and-font-scaling/9366/7
module Xelmish.Forms.FontUtilities

open System
open System.Collections.Generic
open Microsoft.Xna.Framework.Graphics

type private CharacterState =
    {
        Offset: float32*float32;
        Index: int;
        IsFirst: bool;
        IsFinal: bool;
    }

let measureString (text: string) (font: SpriteFont) =
    let measured = font.MeasureString(text)
    measured

let tryGetValue<'K,'V> (dictionary: Dictionary<'K,'V>) key =
    let (exists, value) = dictionary.TryGetValue (key)
    if exists then Some value else None

let getStringCutoff (text: string) (scaleX, scaleY) boundary (font: SpriteFont) =
    let lineHeight = float32 font.LineSpacing
    let spacing = font.Spacing
    let glyphs = font.GetGlyphs()
    let defaultGlyph = 
        font.DefaultCharacter
        |> Option.ofNullable
        |> Option.map (fun c -> tryGetValue glyphs c)
        |> Option.flatten

    let initialCharState = 
        {
            Offset = (0f,0f)
            Index = 0
            IsFirst = true
            IsFinal = false
        }

    text
    |> Seq.indexed
    |> Seq.scan (fun characterState (index, c) -> 
        let cs = { characterState with Index = index }
        match c with
        | '\r' -> cs
        | '\n' ->
            let (ox, oy) = cs.Offset
            { cs with 
                Offset = (0f,oy + lineHeight);
                IsFirst = true 
            }
        | a ->
            let g = 
                tryGetValue glyphs a
                |> Option.orElse defaultGlyph
            match g with
            | None -> raise (ArgumentException(sprintf "Font Sprite was missing a glyph for the character %c" a))
            | Some currentGlyph ->
                let (ox, oy) = cs.Offset
                let (x, first') = 
                    match cs.IsFirst with
                    | true ->
                        (Math.Max(currentGlyph.LeftSideBearing, 0f), false)
                    | false ->
                        (ox + spacing, false)
                let rightMost = ((x + float32 currentGlyph.Cropping.X + currentGlyph.WidthIncludingBearings) * scaleX)
                match rightMost with
                | rm when rm < float32 boundary ->
                    { cs with 
                        Offset = (rm, oy);
                        IsFirst = first';
                    }
                | _ ->
                    { cs with 
                        IsFinal = true;
                    }

    ) initialCharState
    |> Seq.takeWhile (fun c -> not c.IsFinal)
    |> Seq.last
    |> (fun c -> c.Index)
