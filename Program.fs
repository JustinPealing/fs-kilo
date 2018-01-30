open System

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let resetScreen() =
    Console.Clear()
    Console.SetCursorPosition(0,0)

let drawRows() =
    for y in [0..23] do
        printfn "~"

let refreshScreen() =
    resetScreen()
    drawRows()
    Console.SetCursorPosition(0,0)

let processKeypress k =
    match k with
    | Ctrl 'Q' ->
        resetScreen()
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
