module Make(M : sig type t = Atom of string | List of t list  end) = struct


  let to_buffer ~buf sexp =
    let rec loop = function
      | M.Atom str ->
        Buffer.add_string buf (string_of_int (String.length str));
        Buffer.add_string buf ":";
        Buffer.add_string buf str
      | M.List e ->
        Buffer.add_char buf '(';
        ignore (List.map loop e);
        Buffer.add_char buf ')'
    in
    ignore (loop sexp)

  let to_string sexp =
    let buf = Buffer.create 1024 in
    to_buffer ~buf sexp;
    Buffer.contents buf
end

