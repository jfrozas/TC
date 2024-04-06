(* 
función es_afne(automata):
  para cada estado en el automata:
    para cada transición en las transiciones del estado:
      si el símbolo de entrada de la transición es epsilon:
        devolver true
  para cada estado en el autómata:
    para cada transición en las transiciones del estado:
      si el símbolo de entrada de la transición no es epsilon:
        si el estado de destino tiene épsilon-transiciones:
          devolver true
  devolver false 
  
*)
#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;
open Queue;;



type 'a conjunto =
   Conjunto of 'a list
;;

type simbolo =
     Terminal of string
   | No_terminal of string;;


type estado =
   Estado of string;;

type arco_af =
   Arco_af of (estado * estado * simbolo);;

type af = 
   Af of (estado conjunto * simbolo conjunto * estado * arco_af conjunto * estado conjunto);;


let epsilon = af_of_file("/home/javier/Escritorio/Ocaml/ocaml-talf/data/ejemplo01.af");;
let completo = af_of_file("/home/javier/Escritorio/Ocaml/ocaml-talf/data/ejemplo03.af");;
let c = af_of_file("/home/javier/Escritorio/Ocaml/ocaml-talf/data/ejemplo08.af");;
let x = af_of_file("/home/javier/Escritorio/Ocaml/ocaml-talf/data/ejemplo05.af");;

let incompleto = Af
(Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
 Conjunto [Terminal "a"; Terminal "b"], Estado "0",
 Conjunto
  [Arco_af (Estado "0", Estado "0", Terminal "a");
   Arco_af (Estado "0", Estado "1", Terminal "b");
   Arco_af (Estado "1", Estado "0", Terminal "a");
   Arco_af (Estado "1", Estado "2", Terminal "b");
   Arco_af (Estado "2", Estado "0", Terminal "a");
   Arco_af (Estado "2", Estado "3", Terminal "b");
   Arco_af (Estado "3", Estado "3", Terminal "b")],
 Conjunto [Estado "0"; Estado "1"; Estado "2"]);;

 let repetido = Af
 (Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
  Conjunto [Terminal "a"; Terminal "b"], Estado "0",
  Conjunto
   [Arco_af (Estado "0", Estado "0", Terminal "a");
    Arco_af (Estado "0", Estado "1", Terminal "b");
    Arco_af (Estado "1", Estado "0", Terminal "a");
    Arco_af (Estado "1", Estado "2", Terminal "b");
    Arco_af (Estado "2", Estado "0", Terminal "a");
    Arco_af (Estado "2", Estado "3", Terminal "b");
    Arco_af (Estado "3", Estado "2", Terminal "b");
    Arco_af (Estado "3", Estado "3", Terminal "b")],
  Conjunto [Estado "0"; Estado "1"; Estado "2"]);;
 




(* EJERCICIO 1: A *)
(* Comprueba que los arcos no tengan epsilon *)
let es_afne af =
  let arcs = arcos af in
  let rec loop = function
    | [] -> false
    | h :: t -> if simbolos h = "" then true else loop t
  in loop (list_of_conjunto arcs);;

(* Auxiliar que devuelve las transiciones *)
let arcos (Af (_, _, _,transiciones, _)) = transiciones;;

(* Numero maximo de transiciones que puede tener *)
let numero (Af (estados, simbolos, _,_, _)) = 
  List.length(list_of_conjunto(estados)) * List.length(list_of_conjunto(simbolos));;

(* Numero de arcos de un af*)
let n_arcs af = 
  List.length (list_of_conjunto(arcos af));;

(* Parsea el simbolo de un arco *)
let simbolos = function
  |(Arco_af(_,_,Terminal s)) -> s
  |(Arco_af(_,_,No_terminal s)) -> s;; 


let simbol(Arco_af(_,_, s)) = s;; 
(* Si uso esto tb te da el terminal*)



(* Ejercicio 1 b) *)
(* Comprueba que en un mismo estado no haya dos transiciones con el mismo simbolo *)
(* Da false si no presento un no determinismo que no sea epsilon *)
let es_afn af = 
  let arcs = arcos af in
  let rec loop repetidos valor = function

    (*  No pueden salir dos transiciones con el mismo simbolo del mismo estado *)
    | [] -> valor
    (* Se comprueban todas las transiciones *)

    | Arco_af(enter,_,Terminal str) :: t -> 
      (* Hay que ignorar las epsilon, ya que no cuentan *)
      if str = "" then loop repetidos valor t

      (* Si el par, estado entrada, simbolo, ya ha sido metido, se devolvera true (se podria haber devuelto true directo) *)  
      else if pertenece(enter, str) repetidos then 
        loop (agregar (enter, str) repetidos) true t 
      else
        (*Si no, se loopea con la cola (siguiente arco)*)
        loop (agregar (enter, str) repetidos) valor t 
  in loop (Conjunto []) false (list_of_conjunto arcs);;


(*Te da los estados de un automata - La elimine en la entrega sin querer*)
let get_states (Af (s, _, _,_, _)) = s;;

(* Auxiliar para afd *)  
(*Funcion que da True si hay simbolos de entrada duplicados en un estado, o si hay transiciones epsilon, o si lleva a un estado que no existe*)
(* Pone true, pero es falso *)
let es_afn2 af = 
  let arcs = arcos af in
  let rec loop repetidos valor = function

    | [] -> valor

    | Arco_af(enter,out,Terminal str) :: t -> 
      (*False si hay una transicion epsilon*)
      if str = "" then loop repetidos false t

      else if not (pertenece out (get_states af)) then
        false

      else if pertenece(enter, str) repetidos then 
        loop (agregar (enter, str) repetidos) false t 

      else
        loop (agregar (enter, str) repetidos) valor t 

  in loop (Conjunto []) true (list_of_conjunto arcs);;
(*  
Condiciones apra que sea determinista:
-No tenga epsilon transiciones
-No tenga simbolos de entrada repetidos en el mismo estado
-Use todos los símbolos del alfabeto para cada estado (No se si es para completo o determinista simplemente, asi que añado la condición)
-Al aplicar un simbolo, el estado al que se llega ha de pertenecer al autómata   
*)

(*Chequea si es completo, si y solo si no hay arcos repetidos*)
let check_comp af = 
  numero af = n_arcs af;;


(* Ejercicio 1 c) *)
(*Te dice si es afd completo*)
let es_afd af = 
  check_comp af && es_afn2 af;;



(*Ejercicio 2*)
(* Equivalentes *)

(* Empiezas en el inicial (Puede ser un conjunto) *)
(* Chequeas que no hayan sido visitados *)
(* Compruebas que ambos conjuntos tengan la misma finalidad (finales o no finales) *)
(* Si la tienen, calculas el conjunto al que puedes llegar desde ese cojunto para cada uno de los posibles simbolos *)
(* Metes ese conjunto en la cola *)


let equivalentes af1 af2 =

  let Af(e1, s1, ei1, a1, f1) = af1 in
  let Af(e2, s2, ei2, a2, f2) = af2 in

  let rec loop cola visitados  = match cola with

    (* Si la cola esta vacia es que se llego al final, son equivalentes *)
    (* si no, se cogen un par de valores de la cola*)

    |[] -> true 
    |h::t -> let (act1, act2) = h in 

      (* Si ese par de conjuntos ya han sido visitados, ignorar *)
      if pertenece (act1, act2) visitados then loop t visitados

      else 
        (* Si ambos conjuntos son finales o no finales *)
        if (end_conj (list_of_conjunto act1) f1) = (end_conj (list_of_conjunto act2) f2) 

          (* Se llama recursivamente a la funcion, pasandole como parametro la cola,
            anadiendole los estados a los que puedes llegar aplicandole los simbolos posibles, y anadiendo los actuales a visitados*)          
          then loop ((list_of_conjunto (bucle s1 (act1, act2) af1 af2))@cola) (agregar (act1,act2) visitados)

        else false
        
  in loop [( epsilon_cierre (Conjunto [ei1]) af1, epsilon_cierre (Conjunto [ei2]) af2)] (Conjunto []);;


(* Comprueba si hay un estado final en un conjunto  *)
let rec end_conj conjunto1 conjunto2 = 
    match conjunto1 with 
    |[] -> false 
    |h::t -> end_conj t conjunto2 || (pertenece h conjunto2);;


(* Aplica un conjunto de simbolos a un par de estados/conjuntos, y te dice a cuales se puede llegar desde ellos *)
(* Anade a una lista todos los estados a los que se llega sin repetirlos *)
let bucle simbolos estados af1 af2 = agregar (act1,act2) visitados
  let simbolos = list_of_conjunto simbolos in
    let (a, b) = estados in 
    let rec aux simbolos l = match simbolos with
      | [] -> l
      | h::t -> aux t (agregar (mover a h af1, mover b h af2) l)

    in aux simbolos (Conjunto []);;

(* Aplica la formula vista en clase para calcular los estados a los que se puede llegar aplicando un simbolo a un estado *)
let mover est simb af = 
  epsilon_cierre (avanza simb (epsilon_cierre est af) af) af;;



let af2 = Af (Conjunto [Estado "0"; Estado "1"; Estado "2";],
Conjunto [Terminal "a"; Terminal "b";Terminal "c"],
Estado "0",
Conjunto [Arco_af (Estado "0", Estado "0", Terminal "a");
Arco_af (Estado "0", Estado "1", Terminal ""); 
Arco_af (Estado "1", Estado "1", Terminal "b");
Arco_af (Estado "1", Estado "2", Terminal "");
Arco_af (Estado "2", Estado "2", Terminal "c");],
Conjunto [Estado "2"]);;

let af2E = Af (Conjunto [Estado "0"; Estado "1"; Estado "2";],
Conjunto [Terminal "a"; Terminal "b";Terminal "c"],
Estado "0",
Conjunto [Arco_af (Estado "0", Estado "0", Terminal "a");
Arco_af (Estado "0", Estado "1", Terminal "a"); 
Arco_af (Estado "0", Estado "1", Terminal "b");
Arco_af (Estado "0", Estado "2", Terminal "a"); 
Arco_af (Estado "0", Estado "2", Terminal "b");
Arco_af (Estado "0", Estado "2", Terminal "c");
Arco_af (Estado "1", Estado "1", Terminal "b");
Arco_af (Estado "1", Estado "2", Terminal "b");
Arco_af (Estado "1", Estado "2", Terminal "c");
Arco_af (Estado "2", Estado "2", Terminal "c");],
Conjunto [Estado "0"; Estado "1"; Estado "2";]);;


let af2EE = Af (Conjunto [Estado "0"; Estado "1"; Estado "2";Estado "3";],
Conjunto [Terminal "a"; Terminal "b";Terminal "c"],
Estado "0",
Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
Arco_af (Estado "0", Estado "2", Terminal "b"); 
Arco_af (Estado "0", Estado "3", Terminal "c");
Arco_af (Estado "1", Estado "1", Terminal "a"); 
Arco_af (Estado "1", Estado "2", Terminal "b");
Arco_af (Estado "1", Estado "3", Terminal "c");
Arco_af (Estado "2", Estado "2", Terminal "b");
Arco_af (Estado "2", Estado "3", Terminal "c");
Arco_af (Estado "3", Estado "3", Terminal "c");],
Conjunto [Estado "0"; Estado "1"; Estado "2";Estado "3";]);;




