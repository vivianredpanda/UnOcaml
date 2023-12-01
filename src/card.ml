module Card = struct
  type color =
    | Red
    | Blue
    | Green
    | Yellow
    | Any

  type card =
    | Number of int * color
    | Reverse of color
    | Plus of int * color
    | Skip of color
    | Wildcard of color
    | Wildcard4 of color

  (** Returns the color of the card. *)
  let get_color (c : card) : color =
    match c with
    | Reverse col | Skip col | Wildcard col | Wildcard4 col -> col
    | Number (_, col) | Plus (_, col) -> col

  (** Returns Some integer number corresponding to the card. If there does not
      exist any number corresponding with the card, returns None *)
  let get_number (c : card) : int option =
    match c with
    | Reverse _ | Skip _ | Wildcard _ | Wildcard4 _ -> None
    | Number (num, _) | Plus (num, _) -> Some num

  (** Takes in a string and returns the corresponding color. Raises failure if
      string is not a valid color. *)
  let to_color (c : string) : color =
    match String.lowercase_ascii c with
    | "red" -> Red
    | "blue" -> Blue
    | "green" -> Green
    | "yellow" -> Yellow
    | "any" -> Any
    | _ -> failwith "invalid color"

  (** Takes in a color and returns it in string form. *)
  let string_of_color (c : color) : string =
    match c with
    | Red -> "Red"
    | Blue -> "Blue"
    | Green -> "Green"
    | Yellow -> "Yellow"
    | Any -> "Any"

  (* TODO: if input string has no spaces (List.nth will not work) make it not
     raise failure AND if not formatted correctly return some output in terminal
     instead *)

  (** Takes in a string representation of a card, and returns a card. Requires:
      name is in the form "Color Number" (eg: Red 6), "Color Reverse", "Color
      Plus Number" (eg: Yellow Plus 5), "Color Skip", "Color Wildcard" , "Color
      Wildcard4". *)
  let to_card (name : string) : card =
    let lst = String.split_on_char ' ' name |> List.filter (fun x -> x <> "") in
    let c = to_color (List.nth lst 0) in
    let second = List.nth lst 1 in
    match String.lowercase_ascii second with
    | "reverse" -> Reverse c
    | "plus" -> Plus (int_of_string (List.nth lst 2), c)
    | "skip" -> Skip c
    | "wildcard" -> Wildcard c
    | "wildcard4" -> Wildcard4 c
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
