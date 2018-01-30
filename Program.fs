﻿open System
open System.IO
open System.Text

let tabstop = 4

type ERow = {
    chars: string
    render: string
}

type EditorConfig = {
    cx: int; cy: int; rx: int;
    rowoff: int; coloff: int;
    screenrows: int;
    screencols: int;
    rows: ERow[];
}

type AppendBuffer = {
    sb: StringBuilder
}

let abAppend ab (s:string) = 
    ab.sb.Append(s.PadRight(Console.WindowWidth, ' ')) |> ignore

let initEditor() = {
    cx = 0; cy = 0; rx = 0;
    rowoff = 0; coloff = 0;
    screenrows = Console.WindowHeight;
    screencols = Console.WindowWidth;
    rows = [||]
}

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let editorScroll e =
    let rowoff =
        if e.cy < e.rowoff then e.cy
        elif e.cy >= e.rowoff + e.screenrows then e.cy - e.screenrows + 1
        else e.rowoff
    let coloff = 
        if e.rx < e.coloff then e.rx
        elif e.rx >= e.coloff + e.screencols then e.rx - e.screencols + 1
        else e.coloff
    { e with rx = e.cx; rowoff = rowoff; coloff = coloff }

let editorDrawRows e ab =
    for y in [0..e.screenrows - 1] do
        let filterrow = y + e.rowoff;
        if filterrow >= e.rows.Length then
            if e.rows.Length = 0 && y = Console.WindowHeight / 3 then
                let welcomeMessage = "FS-Kilo editor -- version 0.0.1"
                let length = min welcomeMessage.Length e.screencols
                let padding = (e.screencols - length) / 2
                if padding > 0 then
                    abAppend ab ("~".PadRight(padding, ' ') + welcomeMessage)
                else
                    abAppend ab (welcomeMessage.Substring(0, length))
            else
                abAppend ab "~"
        else
            let len = min (max 0 (e.rows.[filterrow].render.Length - e.coloff)) e.screencols
            let start = min e.rows.[filterrow].render.Length e.coloff
            let line = e.rows.[filterrow].render.Substring(start, len)
            abAppend ab line

let editorRefreshScreen e =
    let ab = {sb = new StringBuilder()}
    editorDrawRows e ab

    Console.CursorVisible <- false
    Console.SetCursorPosition(0,0)
    let str = ab.sb.ToString()
    Console.Write(str.Substring(0, str.Length - 1))
    Console.SetCursorPosition(e.rx - e.coloff, e.cy - e.rowoff)
    Console.CursorVisible <- true
    e

let editorMoveCursor e (key:ConsoleKey) = 
    let handlekey e = 
        let rowlen = if e.cy >= e.rows.Length then 0 else e.rows.[e.cy].render.Length
        match key with
        | ConsoleKey.LeftArrow when e.cx > 0 ->
            { e with cx = e.cx - 1 }
        | ConsoleKey.LeftArrow when e.cy > 0 ->
            let cy = e.cy - 1
            { e with cy = cy; cx = e.rows.[cy].render.Length }
        | ConsoleKey.RightArrow when e.cx < rowlen ->
            { e with cx = e.cx + 1 }
        | ConsoleKey.RightArrow when e.cx = rowlen ->
            { e with cy = e.cy + 1; cx = 0 }
        | ConsoleKey.UpArrow when e.cy > 0 ->
            { e with cy = e.cy - 1 }
        | ConsoleKey.DownArrow when e.cy < e.rows.Length ->
            { e with cy = e.cy + 1 }
        | ConsoleKey.PageUp ->
            { e with cy = max 0 (e.cy - e.screenrows) }
        | ConsoleKey.PageDown ->
            { e with cy = min e.rows.Length (e.cy + e.screenrows) }
        | ConsoleKey.Home -> { e with cx = 0 }
        | ConsoleKey.End -> { e with cx = e.screencols - 1 }
        | _ -> e

    let result = handlekey e
    let rowlen = 
        if result.cy >= e.rows.Length then 0
        else e.rows.[result.cy].render.Length
    if result.cx > rowlen then { result with cx = rowlen } else result

let editorProcessKeypress e =
    let c = Console.ReadKey true
    match c.KeyChar with
    | Ctrl 'Q' ->
        Console.SetCursorPosition(0,0)
        Console.Clear()
        exit 0
    | _ ->
        editorMoveCursor e c.Key

let editorRow s = 
    { chars = s; render = s.Replace("\t", "".PadRight(4, ' ')) }

let editorOpen (filename:string) e = 
    { e with rows = File.ReadAllLines filename |> Array.map editorRow }

[<EntryPoint>]
let main argv =
    let rec readloop e = 
        editorScroll e
        |> editorRefreshScreen
        |> editorProcessKeypress
        |> readloop

    initEditor()
    |> if argv.Length > 0 then editorOpen argv.[0] else id
    |> readloop
    0
