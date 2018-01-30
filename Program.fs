open System
open System.Text

let sb = new StringBuilder()

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let drawRows() =
    sb.Clear() |> ignore
    for y in [0..Console.WindowHeight - 1] do
        sb.Append ((sprintf "~").PadRight(Console.WindowWidth, ' ')) |> ignore
    sb.ToString()

let refreshScreen() =
    let rows = drawRows()
    Console.CursorVisible <- false
    Console.SetCursorPosition(0,0)
    Console.Write(rows)
    Console.SetCursorPosition(0,0)
    Console.CursorVisible <- true

let processKeypress k =
    match k with
    | Ctrl 'Q' ->
        Console.SetCursorPosition(0,0)
        Console.Clear()
        exit 0
    | _ -> ()

let rec readloop() = 
    refreshScreen()
    let key = Console.ReadKey true
    processKeypress key.KeyChar
    readloop()

[<EntryPoint>]
let main argv =
    readloop()
    0
