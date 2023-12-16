type lexem = 
  | LCst of string | LVar of string
  | LPlus | LMoins | LFois | LDiv
  | LVrai  | LFaux
  | LOu | LEt | LNon
  | LEgal | LInferieur
  | LRien
  | LAffectation
  | LSi | LAlors | LSinon | LFinSi
  | LTantQue | LFaire | LFinTq
  | LPointVirgule

let motcles = [
  LPlus; LMoins; LFois; LDiv;
  LVrai; LFaux;
  LOu; LEt; LNon;
  LEgal; LInferieur;
  LRien;
  LAffectation;
  LSi; LAlors; LSinon; LFinSi;
  LTantQue; LFaire; LFinTq;
  LPointVirgule
]

let lexique = [
  "+"; "-"; "*"; "/";
  "Vrai"; "Faux";
  "Ou"; "Et"; "Non";
  "="; "<";
  "Rien";
  ":=";
  "Si"; "Alors"; "Sinon"; "FinSi";
  "TantQue"; "Faire"; "FinTq";
  ";"
]

let minuscules = "abcdefghijklmnopqrstuvwxyz"
let decimaux = "0123456789"

type automate = {
  mutable nb : int; (* Nombre d'états *)
  final : (int, lexem) Hashtbl.t;
  delta : (int * char, int) Hashtbl.t
}

let ajouter_transition lexeur (q:int) (a:char) =
  match Hashtbl.find_opt lexeur.delta (q,a) with
    | Some (q') -> q'
    | None -> Hashtbl.add lexeur.delta (q,a) lexeur.nb; lexeur.nb <- lexeur.nb + 1; (lexeur.nb - 1);;

let ajouter_lexeme lexeur (s:string) (l:lexem) =
  let n = String.length s in
  let q = ref 0 in 
  for i = 0 to (n-1) do
    let q' = ajouter_transition lexeur !q s.[i] in
    q := q';
  done;
  Hashtbl.add lexeur.final (!q) l;;
  
let rec add_all_lexem lexeur list_mot list_lex = match list_mot, list_lex with
  | [],[] | [],_ | _,[] -> ()
  | m::mq,l::lq -> ajouter_lexeme lexeur l m; add_all_lexem lexeur mq lq;;

let ajouter_nombre lexeur =
  let qconst = lexeur.nb in
  lexeur.nb <- lexeur.nb + 1;
  for i = 0 to 9 do
    Hashtbl.add lexeur.delta (0,(decimaux.[i])) qconst;
    Hashtbl.add lexeur.delta (qconst,(decimaux.[i])) qconst;
  done;
  Hashtbl.add lexeur.final qconst (LCst(""));;

let ajouter_lettre lexeur =
  let qvar = lexeur.nb in
  lexeur.nb <- lexeur.nb + 1;
  for i = 0 to (String.length minuscules - 1) do
    Hashtbl.add lexeur.delta (0, minuscules.[i]) qvar;
    Hashtbl.add lexeur.delta (qvar, minuscules.[i]) qvar;
  done;
  Hashtbl.add lexeur.final qvar (LVar(""));;

let initialize_automate () =
  let final = Hashtbl.create 25 in
  let delta = Hashtbl.create 60 in
  let lexeur = {nb = 1; final = final; delta = delta} in
  add_all_lexem lexeur motcles lexique;
  ajouter_nombre lexeur;
  ajouter_lettre lexeur;
  lexeur;;

let plus_long_lexeme lexeur s depart =
  let rec aux q i =
    if (depart + i >= String.length s) then None
    else (
      let c = s.[depart + i] in
      let q' = match Hashtbl.find_opt lexeur.delta (q, c) with
        | None -> -1
        | Some(e) -> e in
      if q' = -1 then None
      else (
        match Hashtbl.find_opt lexeur.final q' with
         | None -> aux q' (i+1);
         | Some(lexem) -> let nq = aux q' (i+1) in
         if (nq = None) then Some(i,lexem)
         else nq
      )
    )
  in 
  match aux 0 0 with
    | None -> None
    | Some(i, LCst(_)) -> Some(i, LCst(String.sub s depart (i+1)))
    | Some(i, LVar(_)) -> Some(i, LVar(String.sub s depart (i+1)))
    | e -> e;;


let analyse lexeur s =
  let l = ref [] in
  let n = String.length s in 
  let index = ref 0 in
  while (!index < n) do
    let c = s.[!index] in
    if (c = ' ' || c = '\n' || c = '\t') then index := !index + 1
    else (
      match plus_long_lexeme lexeur s !index with
      | None -> failwith "Lexical error"
      | Some(ni, nlex) -> index := !index + ni + 1; l := nlex :: !l;
    )
  done; List.rev !l

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let write_to_file filename content =
  let output_file = open_out filename in
  output_string output_file content;
  close_out output_file

let lexeur_main () =
  let lexeur = initialize_automate () in
  (* Add lexemes, transitions, and other necessary operations *)
  let file_content = read_file "example.num" in
  let analyzed_content = analyse lexeur file_content in
  (* Process the analyzed content as needed *)
  analyzed_content

let result = lexeur_main ();;