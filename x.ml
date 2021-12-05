open Parser.MenhirInterpreter

(* FIXME remove this, use a single list *)
let string_of_token = function
  | Parser.EOF -> "" (* FIXME? *)
  | Parser.LINT i -> string_of_int i
  | Parser.PLUS -> "+"
  | Parser.LPAR -> "("
  | Parser.RPAR -> ")"

(* FIXME hide this? *)
type t =
  | Success of int
  | Failure of Lexing.position * Parser.token list

let suggest tokens lexer parser s =
  let lexbuf = Lexing.from_string s in
  let success x = Success x in
  let failure c _ = Failure (
    (* FIXME use the second argument to handle things like "+"? *)
    lexbuf.Lexing.lex_curr_p,
    tokens |>
      List.map (fun t -> t, acceptable c t lexbuf.Lexing.lex_curr_p) |>
      List.filter (fun (_, a) -> a (* is true *)) |>
      List.map (fun (t, _) -> t)
  ) in
  let pos = Lexing.{
    pos_fname = "<stdin>";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  } in
  let sup = lexer_lexbuf_to_supplier lexer lexbuf in
  match loop_handle_undo success failure sup (parser pos) with
  | Success _n ->
    []
  | Failure (p, l) ->
    let pos = p.Lexing.pos_cnum in
    l |> List.map @@ fun t ->
      String.sub s 0 pos ^ string_of_token t


open Js_of_ocaml
open Tyxml
(* TODO make this reactive! *)

let () = Js.export "complete" (object%js
  method f (e: Dom_html.keyboardEvent Js.t) =
    match Dom_html.(getElementById_coerce "f" CoerceTo.input) with
    | None -> failwith "f";
    | Some f ->
      let v = Js.to_string f##.value in
      let v' =
        (* Firefox doesn't set onkeypress' keyCode,
         * and some others don't support which *)
        let k = Js.Optdef.get e##.which (fun () -> e##.keyCode) in
        if k >= 32 then
          v ^ String.make 1 (char_of_int k)
        else if k = 8 && v <> "" then
          String.sub v 0 (String.length v - 1)
        else
          v
      in
      let options =
        try
          let tokens = Parser.[PLUS; LINT 42; LPAR; RPAR; EOF] in
          Parser.(suggest tokens Lexer.top Incremental.l v') |>
          List.map @@ fun x ->
            Tyxml_js.Html.(option (pcdata x)) |>
            Tyxml_js.To_dom.of_option
        with _ ->
          []
      in
      (* a different way to do it... meh *)
      let l = Js.Unsafe.coerce (Dom_html.getElementById "l") in
      l##.innerHTML := Js.string "";
      options |> List.iter @@ fun o -> l##appendChild o
end)
