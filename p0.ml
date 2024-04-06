(* EJERCICIO 1 *)

exception Error of string;;

let rec mapdoble func1 func2 list = match list with
  | [] -> raise(Error "Lista vacia")
  | h1::[] -> func1 h1 ::[]
  | h1::h2::tail -> func1 h1 :: func2 h2 :: mapdoble func1 func2 tail;;

(* Indique el tipo de la función mapdoble *)
(* val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun> *)

(* mapdoble (function x -> x) (function x -> -x) [1;1;1;1;1];; *)

(* Valor de: mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;  *)
(* Error: This expression has type string but an expression was expected of type int
Como ya explica el error saltado, las dos funciones aplicadas f1 y f2 dan un resultado de distinto tipo,
y Ocaml no soporta listas de distinto tipado*)

(* Tipo de: let y = function x -> 5 in mapdoble y;; *)
(* - : ('_weak1 -> int) -> '_weak1 list -> int list = <fun> *)
(* La función no define que tipo de dato entra (int, float), pero indica que el argumento y la lista
sean del mismo tipo *)

(* EJERCICIO 2 *)

let rec primero_que_cumple func = function
  | [] -> raise(Error "Lista vacia") 
  | h::tail -> if func h then h else primero_que_cumple func tail;;

(* Tipo de la funcion *)
(* val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun> 

primero_que_cumple (function x -> x>5) [2;7;4;6];;   
*)



let existe func list =
  try
    primero_que_cumple func list; true
  with
  | Error "Lista vacia" -> raise(Error "Lista vacia")
  | _ -> false;;  (* Redundante?? *)

(* Tipo de la funcion *)
(* val existe : ('a -> bool) -> 'a list -> bool = <fun>
existe (function x -> x>5) [2;7;4];;    
*)


(* Para esta funcion se usa snd o fst, sacado de: https://structio.sourceforge.net/guias/progocaml/progocaml.html*)

(* let asociado conj clave = 
  snd(primero_que_cumple (function (a,b) -> a = clave) conj);; *)

(* asociado [(1,false);(2,true);(3,false)] 2;; 

 Estaba usando la funciona nterior, y funciona, pero si no encuentra devuelve "Lista vacia " de la funcion primero_que_cumple
 Asi que hice un cambio
*)

let asociado conj clave = 
  try
    snd(primero_que_cumple (function (a,b) -> a = clave) conj)
  with
  | Error "Lista vacia" -> raise (Error "No se encontro el elemento");;


(* EJERCICIO 3 *)

type 'a arbol_binario =
  Vacio
  | Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;


let arbol = Nodo(3, Nodo(2, Nodo(4, Vacio, Vacio), Nodo(1, Vacio, Vacio)), Nodo(5, Nodo(4, Vacio, Vacio), Nodo(1, Vacio, Vacio)));;

(*
let arbol = Nodo(3, Nodo(2, Nodo(4, Vacio, Vacio), Nodo(1, Vacio, Vacio)), Nodo(5, Nodo(4, Vacio, Vacio), Nodo(1, Vacio, Vacio)));;

in_orden arbol;;
post_orden arbol;;
pre_orden arbol;;
anchura arbol;;
*)


let rec in_orden = function 
  |Vacio -> []
  |Nodo(valor, left, right) -> in_orden left @ [valor] @ in_orden right;;

(*
# in_orden arbol
  ;;
- : int list = [4; 2; 1; 3; 4; 5; 1]   
*)  

let rec pre_orden = function
  | Vacio -> []
  | Nodo(valor, left, right) -> valor::pre_orden left @ pre_orden right;;

(*
# pre_orden arbol
  ;;
- : int list = [3; 2; 4; 1; 5; 4; 1]
*)

let rec post_orden = function 
  | Vacio -> []
  | Nodo(valor, left, right) -> post_orden left @ post_orden right @ [valor];;

(*
# post_orden arbol
  ;;
- : int list = [4; 1; 2; 4; 1; 5; 3]   
*)

let anchura tree = 
  let rec aux l = function
    | [] -> List.rev l
    | Vacio::t -> aux l t
    | Nodo(root,Vacio,Vacio)::t -> aux (root::l) t
    | Nodo(root,h1,h2)::t -> aux (root::l) (t@[h1;h2])
  in aux [] [tree];;


(*
-Define una funcion auxiliar que toma como parámetros una lista vacía y otra lista, que actuará como cola, inicializada con root
-Tras esto, empieza a "loopear" (llamarse recursivamente), hasta que la cola está vacía, momento en el que devuelve la lista al revés
-Si la cola no está vacía:
-Si el root es vacio se ignora
-Si es una hoja con root sin hijos, se añade la root a la lista y se llama a aux con la lista y la cola de la cola
-Si es una hoja con root e hijos, se añade la root a la lista y se llama a aux con la lista y la cola, a la que se añaden los dos hijos

-Por lo general, se ejecutará el 4o caso, se añaden los dos hijos, al explorarlos se añaden sus hijos por orden, y asi hasta que la cola este vacia

*)


(* EJERCICIO 4 *)

(*
let a = Conjunto[];;
let b = Conjunto[1;2;3];;
let c = Conjunto[3;4;5];;   
*)

type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

let rec pertenece a = function 
  | Conjunto [] -> false
  | Conjunto (h::tail) -> if h = a then true else pertenece a (Conjunto tail);;
  (* Si esta vacío, falso, sino, chqueamos que la cabecera sea igual al elemento, y si no, se llama recursivo a pertenece con la cola *)

let agregar a (Conjunto c) = 
  if (pertenece a (Conjunto c) ) then (Conjunto c) else Conjunto (a::c);;
(*Usando la funcion pertenece, miramos si el elemento ya esta en el conjunto, y si no esta, lo añadimos*)

let conjunto_of_list l = 
  let rec conjunto_of_list_aux conj = function
    | [] -> conj
    | h::tail -> conjunto_of_list_aux (agregar h conj) tail
  in conjunto_of_list_aux (Conjunto []) l;;

(* -Se define una funcion auxiliar que va a recibir un conjunto vacío y la lista inicial
-En el caso base (lista vacía, se devuelve el conj (acumulador de los valores)))
-De manera recursiva,s e va añadiendo la cabeza de la lista a conj con la funcion agregar para 
no duplicar elementos
*)

let rec suprimir element = function
  Conjunto [] -> Conjunto []
  | Conjunto (h::tail) -> if h = element then suprimir element (Conjunto tail)
  else agregar h ( suprimir element (Conjunto tail) );;

(*
Si el conjunto está vacío, se devuelve el conjunto tal cual
Si si que está presente, Se comprueba que el primer elemento lo sea:
  -Si lo es, se llama recursivamente a suprimir con la cola (Se elimina la cabeza)
  -Si no lo es, se llama a suprimir con la cola tb, pero añadiendo un agregar con la cabecera, ya que no hay que eliminarla
*)

let cardinal (Conjunto l) = 
  let rec counter c = function
    | [] -> c
    | h::tail -> counter (c+1) tail
  in counter 0 l;;

(*Recorres la lista hasta el final sumando en un contador auxiliar*)


let rec union c = function 
  | Conjunto [] -> c
  | Conjunto (h::tail) -> union (agregar h c) (Conjunto tail);;

(*
-Si el 2o conjunto está vacío, se devuelve el primero
-Si no está vacío, se agrega la cabezadel segundo del conjunto al otro   
*)

let rec interseccion (Conjunto c1) (Conjunto c2) = match c1 with
  [] -> Conjunto []
  | h::tail -> if pertenece h (Conjunto c2) then agregar h (interseccion (Conjunto tail) (Conjunto c2))
  else (interseccion (Conjunto tail) (Conjunto c2));;

(*
Si el primero esta vacío, se devuelve el vacío.
Si no, miramos si su cabeza esta contenida en c2. Si lo está, usamos agregar y llamamos a la interseccion (agregara todos los que se repitan)
Si la cabeza no esta, simplemente llamamos a la interseccion   
*)

let rec diferencia c = function 
  |Conjunto [] -> c
  |Conjunto (h::t) -> if pertenece h c then diferencia (suprimir h c) (Conjunto t)
  else diferencia c (Conjunto t);;

(*
Si el primero está vacío, devolvemos c
Si no, si la h pertenece a c, llamamos recursivamente a diferencia con suprimir de h en c
Si no esta, llamamos a diferencia con c y el conjunto de la t   
*)

let rec incluido c1 c2 = match c1 with
  |Conjunto [] -> true
  |Conjunto (h::tail) -> pertenece h c2 && incluido (Conjunto tail) c2;;

(*
Si están vacíos se da true
Si no, se llama a pertenece h c2, y si true, se llama a incluido tail y c2 recursivamente   
*)

let igual c1 c2 = incluido c1 c2 && incluido c2 c1;;
(*
Si ambos se incluyen son iguales
*)



let producto_cartesiano (Conjunto c1) (Conjunto c2) =

  let rec cp_aux acumulador c1 c2 = match c1 with
    | [] -> Conjunto acumulador
    | x :: xs -> cp_aux (cp_aux2 acumulador x c2) xs c2

  and cp_aux2 acumulador x c2 = match c2 with
    | [] -> acumulador
    | y :: ys -> cp_aux2 ((x, y) :: acumulador) x ys

  in cp_aux [] c1 c2;;

(*
Primer auxiliar-> Si está vacío, devuelve el acumulador (Conjunto)
Si no, Se llama a si mismo con tres parámetros: la segunda auxiliar, su cola y el segundo conjunto
El segundo auxiliar lo que hace es asignar a la x todos los valores de c2, y devolverlo en el acumulador   
*)


let list_of_conjunto (Conjunto l) = l;;

(*
Por la definición de conjunto como 'a list   
*)
    




