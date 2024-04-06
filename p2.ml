(*
#load "talf.cma";;

open Conj;;
open Auto;;
open Ergo;;
open Graf;;
open Queue;;
*)

type 'a conjunto =
   Conjunto of 'a list
;;
let rec pertenece a = function
     Conjunto []     -> false
   | Conjunto (x::l) -> (x = a) || (pertenece a (Conjunto l))
;;
let agregar a (Conjunto l) = 
  if pertenece a (Conjunto l) then
     Conjunto l
  else
     Conjunto (a::l)
  ;;

  let conjunto_vacio =
    Conjunto []
  ;;

  let rec conjunto_of_list = function
  []     -> conjunto_vacio
| (x::l) -> agregar x (conjunto_of_list l)
;;
let cartesiano (Conjunto a) (Conjunto b) =
  Conjunto (List.flatten (List.map (function x -> List.map (function y -> (x,y)) b) a))
;;

let union (Conjunto a) (Conjunto b) =
  conjunto_of_list (a @ b)
;;


type simbolo =
     Terminal of string
   | No_terminal of string;;


let a = Terminal "a";;
let aa = No_terminal "A";;

let palabra1 = [Terminal "b";Terminal "b";Terminal "a";Terminal "b"];;

type regla_gic =
   Regla_gic of (simbolo * simbolo list);;

type gic = 
   Gic of (simbolo conjunto * simbolo conjunto * regla_gic conjunto * simbolo);;


let rule_vali (Conjunto a) (Regla_gic (s, s_set)) = 
  match a with
  | a -> true ;;


let g = gic_of_file("/home/javier/Escritorio/Ocaml/ocaml-talf/data/ejemplo01.gic");;


let rec pertenece a = function
     Conjunto []     -> false
   | Conjunto (x::l) -> (x = a) || (pertenece a (Conjunto l))
;;

let list_of_conjunto (Conjunto l) =
  l
;;


let x = Regla_gic (No_terminal "S", [Terminal "a"; No_terminal "A"]);;
let y = Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "V"]);;
let z = Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "S"]);;
let w = Regla_gic (No_terminal "X", [No_terminal "A"; No_terminal "S"]);;

let palabra = [No_terminal "A";No_terminal "B"];;
let palabra1 = [Terminal "b";Terminal "b";Terminal "a";Terminal "b"];;
let palabra2 = [Terminal "b";Terminal "b";Terminal "a";Terminal "b";Terminal "b";Terminal "b"];;
let palabra1 = [Terminal "b";Terminal "a"];;
let lista = [x;w];;

let g = gic_of_string "S A B C; a b; S;
S -> A B | B C;
A -> B A | a;
B -> C C | b;
C -> A B | a;";;

let g2 = gic_of_string "S A B C D; a b; S;
S -> A B | B C;
A -> B A | a a;
B -> C C | b;
C -> A B | a;";;



let lista2 = [Regla_gic (No_terminal "S", [No_terminal "A"; No_terminal "B"]);
Regla_gic (No_terminal "S", [No_terminal "B"; No_terminal "C"]);
Regla_gic (No_terminal "A", [No_terminal "B"; No_terminal "A"]);
Regla_gic (No_terminal "A", [Terminal "a"]);
Regla_gic (No_terminal "B", [No_terminal "C"; No_terminal "C"]);
Regla_gic (No_terminal "B", [Terminal "b"]);
Regla_gic (No_terminal "C", [No_terminal "A"; No_terminal "B"]);
Regla_gic (No_terminal "C", [Terminal "a"])];;


(* EJERCICIO 1 *)

let rule_valid (Regla_gic (a, s)) =
  match s with
  | [No_terminal _; No_terminal _] -> true
  | [Terminal _] -> true
  | _ -> false;;

let rec initial_valid (Gic(nt, t, (Conjunto r),p)) (Regla_gic (a, s)) = 

  let rec aux nt s = match s with

  | [] -> true
  | h::x -> if h = p then false else aux nt x

in aux nt s;; 

let es_fnc gic = 

  let (Gic(nt, t, (Conjunto r),p)) = gic in 

  let rec loop reglas inicial = match reglas with

    | [] -> true
    | h::t -> if rule_valid h && initial_valid gic h then loop t inicial else false 

  in loop r p;;


  (* *********************************************************** *)

(* EJERCICIO 2 *)

(*Aplica una regla a un simbolo, y devuelve el conjunto si lleva a ese simbolo*)
let rule_simbol (Regla_gic (a, s)) b = 
  if pertenece b (conjunto_of_list s) then (Conjunto [a]) else (Conjunto []);;


(*Aplica un conjunto a un simbolo, y devuelve el conjunto si lleva a ese simbolo*)  
let conj_simbol (Regla_gic (a, s)) conjunto =
  if s = conjunto then (Conjunto [a]) else (Conjunto [])
;;

(* Aplica rule simbol a una lista de reglas *)
let aplicar lista simbol = 
  let rec aux lista simbol acum =
    match lista with
    | [] -> acum
    | h::t -> aux t simbol (union (rule_simbol h simbol) acum)
    in aux lista simbol (Conjunto []);;

(*Aplica conj_simbol a una lista de reglas*)
let aplicar2 lista conjSimbolo = 
  let rec aux lista conjSimbolo acum = 
    match lista with 
    | [] -> acum 
    | h::t -> aux t conjSimbolo (union acum (conj_simbol h conjSimbolo))
  in aux lista conjSimbolo (Conjunto []);;


exception Error of string;;

(*Genera la lista resultante de aplicar el primer paso*)
let primerpaso slist reglas = 
  let rec aux slist reglas acum = match slist with
  | [] -> acum 
  | h::t -> aux t reglas (acum@[aplicar reglas h])
in aux slist reglas [];;

(*Pasa de un par a una lista*)
let cambiar par = match par with 
  | (a,b) -> [a;b];; 

(* Aplica el aplicar2 a una lista de pares de simbolos, que son los resultantes del producto cartesiano de Nik y N(i+k)(j-k
   y devuelve un conjunto con ellos) *)
let productoss lista conjList = 
  let rec loop lista conjList acum = 
    match conjList with
    | [] -> acum
    | h::t -> loop lista t (union acum (aplicar2 lista (cambiar h)))
  in loop lista conjList (Conjunto []);;


let cyk slist gic = 

  let (Gic(nt, t, (Conjunto r),p)) = gic in
  (*Errores*)
  if slist = [] then raise(Error "Lista vacia") else
  if es_fnc gic = false then raise(Error "Not in FNC") else
  (*Ejecutamos el primer paso*)
  let l1 = [primerpaso slist r] in 
  
  (*Este bucle recorre k *)
  let rec bucle3 k conj j i l = if k <= (j-1) then 
    (Printf.printf "%d %d %d \n" j i k ;bucle3 (k+1) (union conj (productoss r (list_of_conjunto( cartesiano (List.nth(List.nth l (k-1)) (i-1)) (List.nth(List.nth l (j-k-1)) (i+k-1))) ) ) ) j i l)
    else conj in
  (* Este bucle recorre i *)
  let rec bucle2 i list j l = if i <= ((List.length slist)-j+1) then 
    bucle2 (i+1) (list@[(bucle3 1 (Conjunto []) j i l)]) j l else list in
  (*Este bucle recorre j y llega al final (mira si el elemento inicial de la gic esta en el elemento de la esquina de la lista de lista de conjuntos)*)
  let rec bucle1 j l = if j <= (List.length slist) then 
    bucle1 (j+1) (l@[(bucle2 1 [] j l)]) else pertenece p (List.nth (List.nth l ((List.length slist)-1)) 0)
  in bucle1 2 l1 
;;  




# cyk palabra1 g;;
2 1 1 1 
2 2 1 1 
2 3 1 1 
3 1 1 2 
3 1 2 2 
3 2 1 2 
3 2 2 2 
Exception: Failure "nth".





productoss lista2 (list_of_conjunto( cartesiano (List.nth(List.nth l2 (1-1)) (3-1)) (List.nth(List.nth l2 (2-1-1)) (3+1-1))) ) ;;






