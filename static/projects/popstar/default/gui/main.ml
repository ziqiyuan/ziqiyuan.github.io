open Ui
open Js_of_ocaml
module Html = Dom_html

(** [js s] converts string s to javastring. *)
let js = Js.string

(** [document] is a webpage opened in a new website window. *)
let document = Html.window##.document

(** [theme] is the theme that the player chooses. 
   It has initial value ["Default"]. **)
let theme = ref "Default"

(** [theme_list] is the list of themes that the player can choose. **)
let theme_list = ["Default"; "Camel"; "Christmas"; "CSLanguage"]

(** [size_input_style input] formats the style of the [input] of board size. *)
let size_input_style input = 
  input##.style##.width := js "30px";
  input##.style##.margin := js "8px";
  input##.style##.height := js "20px";
  input##.style##.padding := js "15px";
  input##.style##.border:= js "1px solid #ccc";
  input##.style##.borderRadius:= js "4px";
  input##.style##.fontSize := js "20px"

(** [int_input name value] is an input fragment with the name [name] and 
    default value [value] attached to the document. *)
let int_input name value =
  let res = document##createDocumentFragment in
  document##.body##.style##.fontSize := js "20px";
  Dom.appendChild res (document##createTextNode (js name));
  let input = Html.createInput ~_type:(js "text") document in
  input##.value := js (string_of_int !value);
  input##.onchange :=
    Html.handler (fun _ ->
        (try 
          let new_value = int_of_string (Js.to_string input##.value) in
          if new_value > 1 && new_value < 15 
          then value := new_value else ()
        with
        | Invalid_argument _ -> ());
        input##.value := js (string_of_int !value);
        Js._false);
  size_input_style input;
  Dom.appendChild res input; 
  res

(** [button_style input] formats the style of a button [input]. *)
let button_style input = 
  input##.style##.backgroundColor := js "#4CAF50";
  input##.style##.border := js "none";
  input##.style##.color := js "white";
  input##.style##.padding := js "15px 30px";
  input##.style##.textAlign := js "center";
  input##.style##.fontSize := js "25px";
  input##.style##.margin := js "8px";
  input##.style##.borderRadius := js "4px"

(** [button n callback] is a button fragment with the name [name] and 
    callback [callback] attached to the document. *)
let button name callback =
  let res = document##createDocumentFragment in
  let input = Html.createInput ~_type:(js "submit") document in
  button_style input;
  input##.value := js name;
  input##.onclick := Html.handler callback;
  Dom.appendChild res input;
  res

(** [dropdown_style input] formats the style of a dropdown list [input]. *)
let dropdown_style input =
  input##.style##.marginRight := js "8px";
  input##.style##.fontSize := js "20px";
  input##.style##.padding := js "5px";
  input##.style##.border := js "1px";
  input##.style##.border:= js "1px solid #ccc"

(** [choose_theme theme_list] is a dropdown fragment from which the 
    player can choose a theme from the [theme_list]. *)
let choose_theme theme_list= 
  let frag = document##createDocumentFragment in 
  Dom.appendChild frag (document##createTextNode (js "Theme: "));
  let input = Html.createSelect document in 
  List.iter 
    (fun selection ->
      let opt = Html.createOption document in
      Dom.appendChild opt (document##createTextNode (js selection));
      Dom.appendChild input opt)
    theme_list;
  dropdown_style input;
  input##.onchange :=
    Html.handler (fun _ ->
      theme := List.nth theme_list (input##.selectedIndex);
      Js._false);
  Dom.appendChild frag input;
  frag

(** [make_start ()] is a fragment containing the introduction to the game. *)  
let make_start () = 
  let title = document##createDocumentFragment in 
  let h1 = Html.createH1 document in 
  h1##.innerHTML := js "Pop Stars!";
  h1##.style##.color := js "brown";
  h1##.style##.fontSize := js "50px";
  let p = Html.createP document in 
  p##.innerHTML := js "Input the size of board [2-14] <br> Before pressing 
  start, DO remember that <br> 1. The more blocks you pop in a single move, 
  the more score you will get. <br> 2. Try to clear all blocks, 
  you will get a lot of bonus. <br> Good luck! ";
  p##.style##.color := js "green";
  p##.style##.fontSize := js "20px";
  Dom.appendChild title h1;
  Dom.appendChild title p;
  title

(** [run_game main size] runs the game on the node [main] with board size
    [size]. *)
let run_game main size mode =
  fun _ ->
    remove_children main;
    let div = Html.createDiv document in
    Dom.appendChild main div;
    run div !size !theme mode;
    Js._false

(** [main_style] formats the style of the main node [main]. *)
let main_style main = 
  main##.style##.position := js "absolute"; 
  main##.style##.top := js "50%";
  main##.style##.left := js "50%";
  main##.style##.transform := js "translate(-50%, -50%)"

let onload _ =
  let size = ref 10 in
  let main =
    Js.Opt.get
      (document##getElementById (js "main"))
      (fun () -> assert false)
  in
  document##.body##.style##.backgroundColor := js "mintcream";
  Dom.appendChild main (make_start ());
  Dom.appendChild main (choose_theme theme_list);
  Dom.appendChild main (int_input "Board Size: " size);
  Dom.appendChild main (Html.createBr document);
  Dom.appendChild main (button "Normal Mode" (run_game main size "Normal"));
  Dom.appendChild main 
      (button "Time Limit Mode" (run_game main size "Countdown"));
  main_style main;
  Js._false

(** Game initiation. *)
let _ = Html.window##.onload := Html.handler onload
