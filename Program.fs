open System

let (|Ctrl|_|) k =
    if Char.IsControl k then Some (char ((int k) ||| 0x40))
    else None

let processKeypress k =
    match k with
    | Ctrl 'Q' -> exit 0
    | _ -> ()

let rec readloop() = 
    let key = Console.ReadKey true
    processKeypress key.KeyChar
    readloop()

[<EntryPoint>]
let main argv =
    readloop()
    0
