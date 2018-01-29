open System

let rec readloop() = 
    let key = Console.ReadKey true
    if Char.IsControl key.KeyChar then
        printfn "%d" (int key.KeyChar)
    else
        printfn "%d ('%c')" (int key.KeyChar) key.KeyChar
    readloop() 

[<EntryPoint>]
let main argv =
    readloop()
    0
