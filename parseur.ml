open Lexeur

type op = Plus | Moins | Mult | Div

type exp_arith =
  | Cst of int
  | Var of string
  | ABin of op * exp_arith * exp_arith

type comp = Egal | Inferieur
type log_op = Et | Ou

type exp_log =
  | Vrai | Faux
  | Comparaison of comp * exp_arith * exp_arith
  | LBin of log_op * exp_log * exp_log
  | Non of exp_log

type program =
  | Rien
  | Affectation of string * exp_arith
  | Sequence of program list
  | Si of exp_log * program * program
  | TantQue of exp_log * program

(* Function to parse arithmetic expressions *)
let rec parse_A (l:lexem list) : (exp_arith*(lexem list)) = match l with
  | [] -> failwith "Erreur parsing Arithm"
  | a::q -> match a with
    | LPlus -> 
      let e1,l1 = parse_A q in
      let e2,l2 = parse_A l1 in
      (ABin(Plus, e1, e2), l2)
    | LMoins -> 
      let e1,l1 = parse_A q in
      let e2,l2 = parse_A l1 in
      (ABin(Moins, e1, e2), l2)
    | LFois -> 
      let e1,l1 = parse_A q in
      let e2,l2 = parse_A l1 in
      (ABin(Mult, e1, e2), l2)
    | LDiv -> 
      let e1,l1 = parse_A q in
      let e2,l2 = parse_A l1 in
      (ABin(Div, e1, e2), q)
    | LCst(v) -> (Cst(int_of_string v), q)
    | LVar(v) -> (Var(v), q)
    | _ -> failwith "Errur parsing Arithm";;

(* Function to parse logical expressions *)
let rec parse_B (l:lexem list) : (exp_log*(lexem list)) = match l with
  | [] -> failwith "Erreur parsing Logique"
  | a::q -> match a with
    | LVrai -> (Vrai, q)
    | LFaux -> (Faux, q)
    | LEgal -> let e1,l1 = parse_A q in
      let e2,l2 = parse_A l1 in
      (Comparaison(Egal, e1, e2), l2)
    | LInferieur -> let e1,l1 = parse_A q in
      let e2,l2 = parse_A l1 in
      (Comparaison(Inferieur, e1, e2), l2)
    | LEt -> let b1,l1 = parse_B q in
      let b2,l2 = parse_B l1 in
      (LBin(Et, b1, b2), l2)
    | LOu -> let b1,l1 = parse_B q in
      let b2,l2 = parse_B l1 in
      (LBin(Ou, b1, b2), l2)
    | LNon -> let b1,l1 = parse_B q in
      (Non(b1), l1)
    | _ -> failwith "Erreur parsing Logique _";;

(* Function to parse programs *)
let rec parse_P (l:lexem list) : (program*(lexem list)) = match l with
  | [] -> (Rien, [])
  | a::q -> match a with
    | LAffectation -> (match q with
      | [] -> failwith "Erreur P Affect"
      | lv::qq -> match lv with
        | LVar(v) -> let a2,l2 = parse_A qq in
          (Affectation(v, a2), l2)
        | _ -> failwith "Erreur P Affectation Not LVar"
    )
    | LSi -> (
      let (lb,q1) = parse_B q in
      match q1 with
        | [] -> failwith "Erreur P Si"
        | alo::q2 -> (
          match alo with
          | LAlors -> (
            let (p1,q3) = parse_Pfinal q2 in
            match q3 with
            | [] -> failwith "Erreur P Alors Empty"
            | sin::q4 -> (
              match sin with
                | LSinon -> (
                  let (p2,q5) = parse_Pfinal q4 in
                  match q5 with
                    | [] -> failwith "Erreur P Sinon Empty"
                    | finsi::q6 -> 
                      match finsi with
                        | LFinSi -> (
                          (Si(lb, p1, p2), q6)
                        )
                        | _ -> failwith "Erreur P FinSi"
                )
                | _ -> failwith "Erreur P Sinon"
            )
          )
          | _ -> failwith "Erreur P Alors"
        )
      )
    | LRien -> (Rien, q)
    | LTantQue -> (
      let (lb,q1) = parse_B q in
      match q1 with 
        | [] -> failwith "Erreur TantQue Empty"
        | fai::q2 -> (
          match fai with
          | LFaire -> (
            let (p1, q3) = parse_Pfinal q2 in
            match q3 with
              | [] -> failwith "Erreur TanQue Faire Empty"
              | ftq::q4 -> (
                match ftq with
                | LFinTq -> (
                  (TantQue(lb, p1), q4)
                )
                | _ -> failwith "Erreur TanQue FinTq"
              )
          )
          | _ -> failwith "Erreur TanQue Faire"
        )
    )
    (* | _ -> let (p1, q1) = parse_P l in
        let (p2, q2) = parse_Pprime q1 in
        print_int 55;
        (Sequence([p1;p2]), q2) *)
    | _ -> (Rien, l)
and parse_Pprime (l:lexem list) : (program*(lexem list)) = match l with
| [] -> (Rien, [])
| a::q -> match a with
  | LRien -> (Rien, q)
  | LPointVirgule -> let (p1,q1) = parse_P q in
    (Sequence([p1]), q1)
  | _ -> failwith "Errur Pprime"

and parse_Pfinal (l:lexem list) : (program*(lexem list)) = match l with
  | [] -> (Rien, [])
  | l -> let p1,l1 = parse_P l in
    let p2,l2 = parse_Pprime l1 in
    (Sequence([p1;p2]),l2);;

let parse ll =
  (* let ll = analyse s in *)
  let rec aux p l = match l with
    | [] -> p
    | l -> (
      let (np, nl) = parse_Pfinal l in
      match p with
      | Sequence(pl) -> (
        match np with
        | Sequence(npl) -> aux (Sequence(pl @ npl)) nl
        | _ -> failwith "IMPOAUSSII"
      )
      | _ -> failwith "IMPO"
    )
  in
  aux (Sequence([Rien])) ll;;

(* Function to convert arithmetic expressions to C string representation *)
let rec arith_to_c (exp:exp_arith) : string = match exp with
  | ABin(opera, e1, e2) -> (
    let sopera = match opera with
      | Plus -> " + "
      | Moins -> " - "
      | Mult -> " * "
      | Div -> " / "
    in
    (arith_to_c e1) ^ sopera ^ (arith_to_c e2) 
  )
  | Cst(n) -> string_of_int n
  | Var(v) -> v;;

(* Function to convert logical expressions to C string representation *)
let rec log_to_c (exp:exp_log) : string = match exp with
| LBin(opera, e1, e2) -> (
  let sopera = match opera with
    | Et -> " && "
    | Ou -> " || "
  in
  (log_to_c e1) ^ sopera ^ (log_to_c e2) 
)
| Non(e) -> "!" ^ (log_to_c e)
| Vrai -> "true"
| Faux -> "false"
| Comparaison(opera, a1, a2) -> 
  let sopera = match opera with
    | Egal -> " == "
    | Inferieur -> " < "
  in
  "(" ^ (arith_to_c a1) ^ sopera ^ (arith_to_c a2) ^ ")";;

(* Function to convert programs to C string representation *)
let rec prog_to_c (p:program) : string = match p with
  | Rien -> ""
  | Affectation(v, e) -> v ^ " = " ^ (arith_to_c e) ^ ";\n"
  | Sequence(pl) -> String.concat "" (List.map prog_to_c pl)
  | Si(e, p1, p2) -> 
    "if (" ^ (log_to_c e) ^ ") {\n" ^ (prog_to_c p1) ^ "}\nelse {\n" ^ (prog_to_c p2) ^ "}\n"
  | TantQue(e, p) -> 
    "while (" ^ (log_to_c e) ^ ") {\n" ^ (prog_to_c p) ^ "}\n";;

(* Function to retrieve variables from a program *)
let rec vars (p:program) : string list =
  let rec aux p = 
    let r = ref [] in
    let _ = 
    match p with
      | Sequence(pl) -> (
        List.iter (fun np -> r := !r @ (vars np)) pl;
      )
      | Affectation(nv,_) -> r := nv::!r
      | _ -> ()
    in 
    !r
  in 
  let f = aux p in
  List.sort_uniq compare f;;

(* Function to compile a program into C code *)
let compile_numerix (p:program) : string = 
  let ss = prog_to_c p in
  let fs = ref "" in
  fs := !fs ^ "#include <stdlib.h>\n";
  fs := !fs ^ "#include <stdio.h>\n";
  fs := !fs ^ "\n int main() {\n";
  let allvars = vars p in
  List.iter (fun sv -> fs := !fs ^ "int " ^ sv ^ ";\n") allvars;
  fs := !fs ^ ss ^ "\n";

  fs := !fs ^ "printf(\"p = %d\\n\", p);\n";

  fs := !fs ^ "\nreturn 0;\n}\n";

  !fs;;

(* Function to perform compilation from input to output file *)
let compilation input_filename output_filename =
  let se = read_file input_filename in
  let lexeur = initialize_automate() in
  let lexemlist = analyse lexeur se in
  let parsed = parse lexemlist in
  let so = compile_numerix parsed in
  write_to_file output_filename so;;

let main () =
  let argc = Array.length Sys.argv in
  let input_filename = if argc > 2 then Sys.argv.(1) else "example.num" in
  let output_filename = if argc > 2 then Sys.argv.(2) else "example.c" in
  compilation input_filename output_filename;;

main();;

