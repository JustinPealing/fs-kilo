open System

let processKeypress (key:ConsoleKeyInfo) = 
    if key.Modifiers.HasFlag(ConsoleModifiers.Control) && key.Key = ConsoleKey.Q then
        exit 0

let rec readloop() = 
    let key = Console.ReadKey true
    processKeypress key
    readloop()

[<EntryPoint>]
let main argv =
    readloop()
    0
