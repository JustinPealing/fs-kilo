open System
open System.Text

let sb = new StringBuilder()

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let editorDrawRows (sb:StringBuilder) =
    for y in [0..Console.WindowHeight - 1] do
        sb.Append ((sprintf "~").PadRight(Console.WindowWidth, ' ')) |> ignore

let editorRefreshScreen() =
    let sb = new StringBuilder()
    editorDrawRows sb

    Console.CursorVisible <- false
    Console.SetCursorPosition(0,0)
    Console.Write(sb.ToString())
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
