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
    | Wildcard of color
    | Wildcard4 of color

  (** Takes in a string and returns the corresponding color. *)
  let to_color (c : string) : color =
    match c with
    | "Red" -> Red
    | "Blue" -> Blue
    | "Green" -> Green
    | "Yellow" -> Yellow
    | _ -> failwith "invalid color"

  (** Takes in a color and returns it in string form. *)
  let string_of_color (c : color) : string =
    match c with
    | Red -> "Red"
    | Blue -> "Blue"
    | Green -> "Green"
    | Yellow -> "Yellow"

  (** Takes in a string representation of a card, and returns a card. Requires:
      name is in the form "Color Number" (eg: Red 6), "Color Reverse", "Color
      Plus Number" (eg: Yellow Plus 5), "Color Skip", "Color Wildcard" , "Color
      Wildcard4". *)
  let to_card (name : string) : card =
    let lst = String.split_on_char ' ' name |> List.filter (fun x -> x <> "") in
    let c = to_color (List.nth lst 0) in
    let second = List.nth lst 2 in
    match second with
    | "Reverse" -> Reverse c
    | "Plus" -> Plus (int_of_string (List.nth lst 3), c)
    | "Skip" -> Skip c
    | "Wildcard" -> Wildcard c
    | "Wildcard4" -> Wildcard c
    | num -> Number (int_of_string num, c)

  (** Takes in a card and returns it in string form, following the requirements
      outlined in to_card. *)
  let string_of_card (car : card) : string =
    match car with
    | Number (num, c) -> string_of_color c ^ " " ^ string_of_int num
    | Reverse c -> string_of_color c ^ " Reverse"
    | Plus (num, c) -> string_of_color c ^ " Plus " ^ string_of_int num
    | Skip c -> string_of_color c ^ " Skip"
    | Wildcard c -> string_of_color c ^ " Wildcard"
    | Wildcard4 c -> string_of_color c ^ " Wildcard4"
end
