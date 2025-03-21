(* Definition des Lens-Typs *)
type ('s, 'v) lens =
  { get : 's -> 'v
  ; put : 'v -> 's -> 's
  }

(* Funktion zum Abrufen des Wertes *)
let view (l : ('s, 'v) lens) : 's -> 'v = l.get

(* Funktion zum Setzen des Wertes *)
let set (l : ('s, 'v) lens) : 'v -> 's -> 's = l.put
