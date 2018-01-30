open System
open System.Text

type AppendBuffer = {
    sb: StringBuilder
}

let abAppend ab (s:string) = 
    ab.sb.Append(s.PadRight(Console.WindowWidth, ' ')) |> ignore

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let editorDrawRows ab =
    for y in [0..Console.WindowHeight - 1] do
        if (y = Console.WindowHeight / 3) then
            let welcomeMessage = "FS-Kilo editor -- version 0.0.1"
            if welcomeMessage.Length > Console.WindowWidth then
                abAppend ab (welcomeMessage.Substring(0, Console.WindowWidth))
            else
                abAppend ab welcomeMessage
        else
            abAppend ab "~"

let editorRefreshScreen() =
    let ab = {sb = new StringBuilder()}
    editorDrawRows ab 

    Console.CursorVisible <- false
    Console.SetCursorPosition(0,0)
    Console.Write(ab.sb.ToString())
    Console.SetCursorPosition(0,0)
    Console.CursorVisible <- true

let editorProcessKeypress() =
    let c = (Console.ReadKey true).KeyChar
    match c with
    | Ctrl 'Q' ->
        Console.SetCursorPosition(0,0)
        Console.Clear()
        exit 0
    | _ -> ()

[<EntryPoint>]
let main argv =
    let rec readloop() = 
        editorRefreshScreen()
        editorProcessKeypress()
        readloop()
    readloop()
    0
