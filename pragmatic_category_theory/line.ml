type t =
  { chunks : Chunk.t list
  ; length : int
  }

let append line1 line2 =
  let chunks = line1.chunks @ line2.chunks in
  let length = line1.length + line2.length in
  { chunks; length }
;;
