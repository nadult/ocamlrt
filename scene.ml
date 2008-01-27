(* Definicje obiektów sceny + funkcje ³aduj¹ce *)

open Base;;
open Scanf;;
open Printf;;

(* pozycja; kolor; objêtoœæ; gêstoœæ *)
type light =	float array * float array *
				float array * int array;;

type mat_texture =
				Standard_texture of Texture.texture * float * float	|	(* teksturka; skala u; skala v *)
				Empty_texture;;											(* bez teksturki *)
				
type material = float array * mat_texture * mat_texture * float;;	(* kolor, tekstura koloru, normalmapa, wart. odbicia *)

let empty_material =
	let tex = Empty_texture in
	((vec 1.0 1.0 1.0),tex,tex,0.);;
				
type entity =	Sphere of float array * float			(* pozycja; promieñ *)
				| Plane of float array * float			(* wektor normalny; odleg³oœæ od 0,0,0 *)
				| Triangle of float array * float array (* p1, p2, p3, plaszczyzna (nrm, d) *)
					* float array * float array * float
				| KDNode of float * int * (entity*material) list (* pozycja, os, lista dzieci *)
				| Dummy;;


type material_t = ( (float array) -> (float array) ) *              (* uv -> color *)
                  ( (float array*float array) -> (float array) );;  (* uv nrm -> bumped nrm *)

type collision_t =  No_collision |                              (* Brak kolizji *)
                    Collision of float * entityref_t            (* odl. kolizji od ray_orig; odnosnik do collidera *)
                and
     entityref_t =  Entityref_this |
                    Entityref of entity_t
                and
     entity_t = ( (float array*float array)->collision_t) *     (* ray -> collision *)
                ( (float array) -> (float array) ) *            (* collision pos -> normal *)
                ( (float array) -> (float array) ) *            (* collision pos -> color *)
                ( float array * float array );;                 (* bbox min, bbox max *)


let extract_entityref entbase entref =
    match entref with
    Entityref_this  -> entbase
    | Entityref(ent)-> ent
;;

(* bierze pare entity,material *)
let entity_pos ent =
	let e,m = ent in
	match e with
	| Triangle(p1,p2,p3,_,_)	-> (p1+|p2+|p3)*|(1./.3.)
	| Sphere(pos,_)				-> pos
;;

(* bierze pare entity,material *)
let entity_rad ent =
	let e,m = ent in
	match e with
	| Triangle(p1,p2,p3,_,_)	-> let center=(p1+|p2+|p3)*|(1./.3.) in
									let l1,l2,l3=length (p1-|center),length (p2-|center),length (p3-|center) in
									max (max l1 l2) l3
	| Sphere(_,rad)				-> rad
;;


let entity_min ent =
    let r=entity_rad ent in
    (entity_pos ent) -| [| r; r; r |]
;;

let entity_max ent =
    let r=entity_rad ent in
    (entity_pos ent) +| [| r; r; r |]
;;

(* Na wejsciu dostaje liste par ent,mat (skonczonych) *)
let rec make_kdtree objects axis deep =
	let sorted = List.sort (fun a b -> int_of_float (((entity_pos a).(axis)) -. ((entity_pos b).(axis))) ) objects in
	let min,max = List.fold_left
		( fun (mi,ma) e -> let p,r=entity_pos e,entity_rad e in (minv mi (p-|(vec r r r))),(maxv ma (p+|(vec r r r))) )
		((vec 0. 0. 0.),(vec 0. 0. 0.)) sorted in
	let pos = ((min +| max) *| 0.5).(axis) in
	if ((List.length sorted) < 4) || deep>5
	then (* malo obiektow *)
		( KDNode( pos, 3, sorted ), empty_material)
	else (* dzielimy dalej *)
        let leftSide = List.fold_left (fun lst e -> if ((entity_min e).(axis)) < pos then e::lst else lst) [] sorted in
        let rightSide = List.fold_left (fun lst e -> if ((entity_max e).(axis)) > pos then e::lst else lst) [] sorted in

        let leftk = make_kdtree leftSide ((axis+1) mod 3) (deep+1) in
        let rightk = make_kdtree rightSide ((axis+1) mod 3) (deep+1) in

		(KDNode( pos, axis, [leftk; rightk] ), empty_material )
;;

let rec print_kdtree pos axis lst =
	printf "kd %d %.1f( " axis pos;
	List.fold_left (fun _ (obj,mat) -> match obj with Sphere(_,_) -> printf "sp
    " | Triangle(_,_,_,_,_) -> printf "tri " | KDNode(p,a,lst) -> print_kdtree p a lst )
				() lst;
	printf ") ";
;;

let print_scene scn =
	List.fold_left (fun _ (obj,mat) -> match obj with Plane(_,_) -> printf "pln
    " | KDNode(p,a,lst) -> print_kdtree p a lst )
				() scn;
	printf "\n";
;;

(* Wpakowuje obiekty skonczone do drzewa bvh, reszty sie nie tyka *)
let optimize_scene objects =
	let planes,rest = List.partition ( fun (e,m) -> match e with Plane(_,_) -> true | a -> false ) objects in
	let output = (make_kdtree rest 0 0) :: planes in
	(* print_scene output; *)
	output
;;

(* £aduje scenê z wejœcia; Wymaga podania listy tekstur *)			
let load_entities materials =
	let objects_list =
	parse_list "objects" ([]: (entity*material) list)
		(fun lst ->
			let ent = (		(* £adowanie obiektu *)
			
				let etype = scanf "%s" id in
				
				match etype with
					"sphere"	->
						let x,y,z,r = scanf "\t(%f,%f,%f) %f\t\t\t" (fun a b c d-> a,b,c,d) in
						Sphere ( [| x; y; z |], r )
		(*			| "box"		->
						let x,y,z,sx,sy,sz = scanf "\t(%f,%f,%f) (%f,%f,%f)\t\t\t" (fun a b c d e f-> a,b,c,d,e,f) in
						Box ( [| x; y; z |], [| sx; sy; sz |] )  *)
					| "plane"	->
						let nx,ny,nz,d = scanf "\t(%f,%f,%f) %f\t\t\t" (fun a b c d-> a,b,c,d) in
						Plane ( unitize [| nx; ny; nz |], d )
					| "tri"		->
						let p1,p2,p3 = scanf "\t(%f,%f,%f) (%f,%f,%f) (%f,%f,%f)\t\t\t" (fun a b c d e f g h i-> (vec a b c), (vec d e f), (vec g h i)) in
						let nrm = unitize (cross (p2 -| p1) (p3 -| p1)) in
						let dst = dot nrm p1 in
						Triangle ( p1,p2,p3,nrm,dst )
					| _		-> failwith "Unknown object type"
					
			) in let mat = (	(* Ladowanie materialu *)
			
                let mname = scanf "%s" id in

				let rec find_mat tname tlist =
					match tlist with
					[]		    	-> empty_material
					| (nm,m)::t		-> if nm=tname then m else find_mat tname t
				in find_mat mname materials
            )
			in (ent, mat) :: lst
		) in
    objects_list
;;

let gen_sphereflake start_pos start_size start_level material =
    let rec inner lst level pos size2 =
        if level=0 then lst else

        let t = (Sphere( pos, size2 ), material) :: lst in let size = size2/.2. in
        let move = size2+.size*.1.1 in
        let t1 = inner t  (level-1) [| (pos.(0))-.move; pos.(1); pos.(2) |] size in
        let t2 = inner t1 (level-1) [| (pos.(0))+.move; pos.(1); pos.(2) |] size in
        let t3 = inner t2 (level-1) [| pos.(0); (pos.(1))-.move; pos.(2) |] size in
        let t4 = inner t3 (level-1) [| pos.(0); (pos.(1))+.move; pos.(2) |] size in
        let t5 = inner t4 (level-1) [| pos.(0); pos.(1); (pos.(2))-.move |] size in
        let t6 = inner t5 (level-1) [| pos.(0); pos.(1); (pos.(2))+.move |] size in
        t6
    in
    inner [] start_level start_pos start_size
;;

let load_materials textures =
    parse_list "materials" ([]: (string*material) list)
        (fun lst ->
            let name = scanf "%s " id in
            let color = scanf "(%f,%f,%f) " (fun a b c -> vec a b c) in
			let tex_name = scanf "%s" id in let u1,v1 = if tex_name="-" then 0.,0. else scanf " (%f,%f)" (fun a b -> a,b) in
			let nrm_name = scanf " %s" id in let u2,v2 = if nrm_name="-" then 0.,0. else scanf " (%f,%f)" (fun a b -> a,b) in
			let refl_val = scanf " %f" id in
			
			let rec mat_tex tname tlist u v =
				match tlist with
				[]			-> Empty_texture
				| h::t		-> let _,_,_,name=h in
					if name=tname
						then Standard_texture (h,u,v)
						else mat_tex tname t u v
			in
			let tex1,tex2=(mat_tex tex_name textures u1 v1),(mat_tex nrm_name textures u2 v2) in
			( name, ( color, tex1, tex2, refl_val )) :: lst
        )
;;

let load_lights (_:unit) =
	parse_list "lights" ([]: light list)
		(fun lst ->
			let volume,density = ( match scanf "%s" id with
				"point" ->	[|0.;0.;0.|],[|1;1;1|]
				| "box" -> scanf " {(%f,%f,%f),(%d,%d,%d)}" (fun a b c d e f->[|a;b;c|],[|d;e;f|])
				| _		-> failwith "Unknown light type" ) in
			let px,py,pz,r,g,b = scanf " (%f,%f,%f) (%f,%f,%f)" (fun a b c d e f->a,b,c,d,e,f) in
			( [| px; py; pz |],[| r; g; b |],volume,density ) :: lst
		)
;;
