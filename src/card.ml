module Card = struct
  type color =
    | Red
    | Blue
    | Green
    | Yellow

  type card =
    | Number of int * color
    | Reverse of color
    | Plus of int * color
    | Skip of color
    | Wildcard
    | Wildcard4

  let to_card (name : string) : card = failwith ("" ^ name)
end
