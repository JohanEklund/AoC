module ComputerListUtils

let UpdatedValue newValue oldValue currentIndex wantedIndex =
    match currentIndex = wantedIndex with
    | true -> newValue
    | false -> oldValue

let replaceValueAt list index newValue = 
    list
    |> Seq.mapi (fun i elem -> UpdatedValue newValue elem i index)
    |> Seq.toList