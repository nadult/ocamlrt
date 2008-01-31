(* Typy i operacje bazowe *)

type vector = float array;;

let clamp v a b = if v < a then a else if v > b then b else v
let id x = x
let sqr x = x *. x

let vec x y z = [| x; y; z; |]
let ( *| ) v s = [| v.(0)*.s; v.(1)*.s; v.(2)*.s |]
let ( +| ) v1 v2 = [| v1.(0)+.v2.(0); v1.(1)+.v2.(1); v1.(2)+.v2.(2) |]
let ( -| ) v1 v2 = [| v1.(0)-.v2.(0); v1.(1)-.v2.(1); v1.(2)-.v2.(2) |]

let dot v1 v2 = v1.(0)*.v2.(0) +. v1.(1)*.v2.(1) +. v1.(2)*.v2.(2)
let cross v1 v2 =
	[|	((v1.(1))*.(v2.(2)))-.((v1.(2))*.(v2.(1)));
		((v1.(2))*.(v2.(0)))-.((v1.(0))*.(v2.(2)));
		((v1.(0))*.(v2.(1)))-.((v1.(1))*.(v2.(0)))  |]

let length_sq v = dot v v
let length v = sqrt (length_sq v)
let unitize v = let mul = 1. /. (length v) in [| v.(0) *. mul; v.(1) *. mul; v.(2) *. mul |]

let minv a b = vec (min (a.(0)) (b.(0))) (min (a.(1)) (b.(1))) (min (a.(2)) (b.(2)))
let maxv a b = vec (max (a.(0)) (b.(0))) (max (a.(1)) (b.(1))) (max (a.(2)) (b.(2)))
let mulv v1 v2 = [| v1.(0)*.v2.(0); v1.(1)*.v2.(1); v1.(2)*.v2.(2) |]

let transpose mat =
	let u,v,w=mat.(0),mat.(1),mat.(2) in
	let x1,y1,z1=u.(0),u.(1),u.(2) in
	let x2,y2,z2=v.(0),v.(1),v.(2) in
	let x3,y3,z3=w.(0),w.(1),w.(2) in
	vec (vec x1 x2 x3) (vec y1 y2 y3) (vec z1 z2 z3)

let transform v m33 = [| dot v (m33.(0)); dot v (m33.(1)); dot v (m33.(2)) |]

let vec2color vec =
	let cr = clamp (int_of_float (vec.(0)*.255.)) 0 255 in
	let cg = clamp (int_of_float (vec.(1)*.255.)) 0 255 in
	let cb = clamp (int_of_float (vec.(2)*.255.)) 0 255 in
	Graphics.rgb cr cg cb
;;

let reflect r nrm =
	r -| nrm*|((dot nrm r)*.2.);;


open Scanf;;
open Printf;;
	
(* Parsuje liste obiektow postaci:

nazwa_typu_listy {\n
\t	obiekt\n
\t	obiekt\n
\t	obiekt\n
...
}\n	

funkcja pobiera jeden parametr: wynik wyliczenia funkcji dla poprzedniego obiektu (albo a)
*)
let parse_list list_name a func =
	let nomore,out = ref false,ref a in
	( if (scanf "\n%s {" id) <> list_name then failwith ("Expected "^list_name^" list") );
	
	while not !nomore do
		match scanf "\n%c" id with
		'\t'	-> out := func !out;
		| '}'	-> let _=scanf "%c" id (* ma byæ \n *) in nomore := true;
		| _		-> failwith ("Syntax error while defining "^list_name);
	done;
	!out
;;

(* pozycja; kolor; objêtosc; gêstosc *)
type light =	float array * float array *
				float array * int array;;

(* resx; resy; tablica wartosci; nazwa *)
type texture_t = int * int * (float array array) * string;;

type mat_texture =
				Standard_texture of texture_t * float * float	|	(* teksturka; skala u; skala v *)
				Empty_texture;;										(* bez teksturki *)

type material_t = ( (float array) -> (float array) ) *              (* uv -> color *)
                  ( float array->float array -> (float array) );;  (* uv nrm -> bumped nrm *)

type ray_t = float array * float array;;
type tracedata_t = float array * float * float;;                (* invdir; mint; maxt *)

type collision_t =  No_collision |                              (* Brak kolizji *)
                    Collision of float * entityref_t            (* odl. kolizji od ray_orig; odnosnik do collidera *)
                and
     entityref_t =  Entityref_this |
                    Entityref of entity_t
                and
     entity_t = ( ray_t->tracedata_t ->collision_t) *           (* ray,trace_data -> collision *)
                ( (float array) -> (float array) ) *            (* collision pos -> normal *)
                ( (float array) -> (float array) ) *            (* collision pos -> color *)
                ( float array * float array );;                 (* bbox min, bbox max *)


let extract_entityref (entbase:entity_t) (entref:entityref_t) =
    match entref with
    Entityref_this  -> Entityref(entbase)
    | Entityref(ent)-> Entityref(ent)
;;

let create_collision dist =
    if dist < 0.
    then No_collision
    else Collision ( dist, Entityref_this )
;;

