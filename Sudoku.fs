module Sudoku
type Sudoku = S of int list list

let make l = S(l)
let listify (S(l)) = l

let print s =
  let stringifyDigit n = if n=0 then " " else string n
  let rec stringifyRow = function
    | [] -> "|"
    | n::ns ->
        if (List.length ns) % 3 = 2
          then "| " + (stringifyDigit n) + " " + (stringifyRow ns)
        else (stringifyDigit n) + " " + (stringifyRow ns)
  let rec printRows = function
    | [] -> (printfn "    - - -   - - -   - - -  ")
    | row::rows ->
        if (List.length rows) % 3 = 2 then
          (printfn "    - - -   - - -   - - -  ")
        printfn "  %s" (stringifyRow row)
        printRows rows
  printRows (listify s)

let isValid s =
  let rec isValidRow = function
    | [] -> true
    | (n:int)::ns ->
        if n <> 0 && List.exists (fun x -> x=n) ns then false
        else isValidRow ns
  let checkRows rows = List.fold (fun c row -> c && (isValidRow row)) true rows
  let rec checkRows = function
    | [] -> true
    | row::rows -> rowChecker row && checkRows rows
  let rec transpose = function
    | []::_ -> []
    | rows ->
        let (column, rest) =
          List.foldBack (fun row (result, rest) ->
                          let n::ns = row
                          (n::result, ns::rest))
                        rows ([], [])
        column :: (transpose rest)
  let rec rowify3Boxes = function
    | [] -> [[];[];[]]
    | n0::n1::n2::ns ->
        let [b0;b1;b2] = rowify3Boxes ns
        [ n0::n1::n2::b2; b0; b1 ]
  let rec rowifyBoxes = function
    | [] -> []
    | r0::r1::r2::rs ->
        (rowify3Boxes (r0 @ r1 @ r2)) @ (rowifyBoxes rs)
  let l = listify s
  checkRows l && checkRows (transpose l) && checkRows (rowifyBoxes l)

let isDone s =
  isValid s && List.sum (List.map List.sum (listify s)) = 405
