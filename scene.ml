(* Definicje obiektów sceny + funkcje ³aduj¹ce *)

open Base;;
open Scanf;;
open Printf;;

(* Laduje scenê z wejscia; Wymaga podania listy materialow *)
let load_entities materials =
	let objects_list =
	parse_list "objects" ([]: entity_t list)
		(fun lst ->
			let ent = (		(* £adowanie obiektu *)
			
				let etype = scanf "%s" id in
				
                (* funkcja pobiera material jako parametr *)
                let func = ( match etype with
					"sphere"	->
						let x,y,z,r = scanf "\t(%f,%f,%f) %f\t\t\t" (fun a b c d-> a,b,c,d) in
                        ( fun mat -> Sphere.create (vec x y z) r mat )
		(*			| "box"		->
						let x,y,z,sx,sy,sz = scanf "\t(%f,%f,%f) (%f,%f,%f)\t\t\t" (fun a b c d e f-> a,b,c,d,e,f) in
						Box ( [| x; y; z |], [| sx; sy; sz |] )  *)
					| "plane"	->
						let nx,ny,nz,d = scanf "\t(%f,%f,%f) %f\t\t\t" (fun a b c d-> a,b,c,d) in
                        ( fun mat -> Plane.create (unitize (vec nx ny nz)) d mat )
					| "tri"		->
						let p1,p2,p3 = scanf "\t(%f,%f,%f) (%f,%f,%f) (%f,%f,%f)\t\t\t"
                            (fun a b c d e f g h i-> (vec a b c), (vec d e f), (vec g h i)) in
                        ( fun mat -> Triangle.create p1 p2 p3 mat )
					| _		-> failwith "Unknown object type"
                ) in
					
			    (func:(material_t->entity_t)) (	(* Ladowanie materialu / tworzenie obiektu *)
                    let mname = scanf "%s" id in

	    			let rec find_mat tname tlist =
		    			match tlist with
			    		[]		    	-> Material.empty
				    	| (nm,m)::t		-> if nm=tname then m else find_mat tname t
    				in find_mat mname materials )
            )
			in ent :: lst
		) in

    Entlist.create objects_list

	(*let planes,rest = List.partition (fun (_,_,_,(_,b)) -> ( (b.(0))=infinity )) objects_list in
	Entlist.create ((Kdtree.create rest) :: planes)*)
;;

(*

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
*)

let load_materials textures =
    parse_list "materials" ([]: (string*material_t) list)
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
			( name, (Material.create color tex1 tex2 refl_val) ) :: lst
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
