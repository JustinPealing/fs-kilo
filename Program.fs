open System
open System.Text

type EditorConfig = {
    cx: int;
    cy: int;
}

type AppendBuffer = {
    sb: StringBuilder
}

let abAppend ab (s:string) = 
    ab.sb.Append(s.PadRight(Console.WindowWidth, ' ')) |> ignore

let mutable e = { cx = 0; cy = 0 }

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let editorDrawRows ab =
    for y in [0..Console.WindowHeight - 1] do
        if (y = Console.WindowHeight / 3) then
            let welcomeMessage = "FS-Kilo editor -- version 0.0.1"
            let length = min welcomeMessage.Length Console.WindowWidth
            let padding = (Console.WindowWidth - length) / 2
            if padding > 0 then
                abAppend ab ("~".PadRight(padding, ' ') + welcomeMessage)
            else
                abAppend ab (welcomeMessage.Substring(0, length))
        else
            abAppend ab "~"

let editorRefreshScreen() =
    let ab = {sb = new StringBuilder()}
    editorDrawRows ab 

    Console.CursorVisible <- false
    Console.SetCursorPosition(0,0)
    Console.Write(ab.sb.ToString())
    Console.SetCursorPosition(e.cx, e.cy)
    Console.CursorVisible <- true

let editorMoveCursor (key:ConsoleKey) = 
    match key with
    | ConsoleKey.LeftArrow -> e <- { e with cx = e.cx - 1 }
    | ConsoleKey.RightArrow -> e <- { e with cx = e.cx + 1 }
    | ConsoleKey.UpArrow -> e <- { e with cy = e.cy - 1 }
    | ConsoleKey.DownArrow -> e <- { e with cy = e.cy + 1 }
    | _ -> ()

let editorProcessKeypress() =
    let c = Console.ReadKey true
    match c.KeyChar with
    | Ctrl 'Q' ->
        Console.SetCursorPosition(0,0)
        Console.Clear()
        exit 0
    | _ -> editorMoveCursor c.Key

[<EntryPoint>]
let main argv =
    let rec readloop() = 
        editorRefreshScreen()
        editorProcessKeypress()
        readloop()
    readloop()
    0
