(* G³ówne procedurki licz¹ce *)

open Base;;
open Scene;;

type ray = float array * float array
					
let ray_sphere r s_pos s_rad =
	let r_orig,r_dir = r in
	
	let dst = r_orig -| s_pos in
	let b = dot dst r_dir in
	let c = (dot dst dst) -. (s_rad*.s_rad) in
	let d = b *. b -. c in
	if d > 0. then -. b -. (sqrt d) else infinity
;;

let ray_plane r p_nrm p_dist =
	let r_orig,r_dir = r in

	let cosa = dot r_dir p_nrm in
	if (if cosa < 0. then -.cosa else cosa) < 0.000001 then infinity
	else (p_dist -. (dot r_orig p_nrm)) /. cosa
;;

open Printf;;


let ray_triangle r p1 p2 p3 nrm d =
	let dist = ray_plane r nrm d in
	if dist <> infinity then
		let r_orig,r_dir = r in
		let hit=r_orig+|(r_dir*|dist) in
		let side1 = dot (cross (p2-|p1) nrm) (hit-|p1) in
		let side2 = dot (cross (p3-|p2) nrm) (hit-|p2) in
		let side3 = dot (cross (p1-|p3) nrm) (hit-|p3) in
		if side1<0.&&side2<0.&&side3<0. then dist else infinity
		
	else infinity
;;

let ray_ent r e =
	let out = (
		match e with
		Sphere(pos,rad)			 	-> ray_sphere r pos rad
		| Plane(nrm,dist)		 	-> ray_plane r nrm dist
		| Triangle(p1,p2,p3,nrm,d)	-> ray_triangle r p1 p2 p3 nrm d
	) in
	if out < 0. then infinity else out
;;

(* zwraca t_collision *)
type t_collision =	No_collision |
				 	Collision of float * entity * material;;

let ray_entc r e m =
    let d=ray_ent r e in
    if d==infinity then No_collision
    else Collision(d,e,m)
;;

let min_col a b =
    match a with
    No_collision            -> b
    | Collision(ap,ae,am)   -> (
            match b with
            No_collision            -> a
            | Collision(bp,be,bm)   ->
                if ap < bp then Collision(ap,ae,am) else Collision(bp,be,bm)
    )
;;

let rec ray_kdtree_ r (r_orig,r_idir) (pos,axis,list) (min,max) =
    if axis==3 then
        List.fold_left (fun col (e,m) -> min_col col (ray_entc r e m) ) No_collision list
    else (
        let orig,idir = (r_orig.(axis)),(r_idir.(axis)) in
        let tpos = (pos-.orig)*.idir in
        let lp, la, ll, rp, ra, rl = ( if idir >= 0.
            then ( match list with (KDNode(a,b,c),_)::(KDNode(d,e,f),_)::_ -> a,b,c,d,e,f ) 
            else ( match list with (KDNode(a,b,c),_)::(KDNode(d,e,f),_)::_ -> d,e,f,a,b,c )
        ) in

        if max<tpos then
            ray_kdtree_ r (r_orig,r_idir) (lp,la,ll) (min,tpos)
        else if min>tpos then
            ray_kdtree_ r (r_orig,r_idir) (rp,ra,rl) (tpos,max)
        else (
            let lcol = ray_kdtree_ r (r_orig,r_idir) (lp,la,ll) (min,tpos) in
            let rcol = ray_kdtree_ r (r_orig,r_idir) (rp,ra,rl) (tpos,max) in
            min_col lcol rcol
        )
    )
;;

let ray_kdtree r (pos,axis,list) =
    let r_orig,r_dir = r in let r_idir = [| 1./.(r_dir.(0)); 1./.(r_dir.(1)); 1./.(r_dir.(2)) |] in
    ray_kdtree_ r (r_orig,r_idir) (pos,axis,list) (0.,10000.)
;;

(* Zwraca parê odleg³oœæ,obiekt *)
let rec closest_object r s =
	match s with
	(e,m)::t		->
		let col1 = (
            match e with
			KDNode(pos,axis,list)	-> ray_kdtree r (pos,axis,list)
			| _                     -> ray_entc r e m
		) in min_col col1 (closest_object r t)
    | []			->  No_collision
;;

(* col - pozycja kolizji *)
let entity_normal ent col =
	match ent with
	Plane(nrm,_) 			-> nrm
	| Triangle(_,_,_,nrm,_)	-> nrm
	| Sphere(pos,_)			-> unitize (col -| pos)
;;

(* Wylicza koordynaty tekstury dla danego obiektu *)
let entity_texcoord ent col uscale vscale =
	match ent with
	Plane(nrm,_)			->	((col.(0))*.uscale),(((col.(1))+.(col.(2)))*.vscale)
	| Triangle(_,_,_,nrm,_)	->	((col.(0))*.uscale),(((col.(1))+.(col.(2)))*.vscale)
	| Sphere(pos,rad)		-> 	(asin ((((col.(0)-.pos.(0))+.(col.(2)-.pos.(2))))/.(rad*.2.)))*.uscale*.2.,
								(asin (( col.(1)-.pos.(1))/.rad) *. vscale)
;;

(* oblicza ostateczn¹ normaln¹ bior¹c pod uwagê mapê normalnych *)
let compute_normal ent nrm_tex col_pos =
	let base_nrm = entity_normal ent col_pos in
	match nrm_tex with
	Empty_texture				-> base_nrm
	| Standard_texture(tex,u,v) ->
		let tex_val = (Texture.sample tex (entity_texcoord ent col_pos u v)) in
		let tex_nrm = (tex_val *| 2.) -| (vec 1. 1. 1.) in
		let tg1 = vec (-.(base_nrm.(1))) (base_nrm.(0)) (base_nrm.(2)) in let tg2=cross base_nrm tg1 in
		transform tex_nrm (transpose (vec tg1 tg2 base_nrm))
;;

(* oblicza wartoœæ tekstury danego obiektu w danym punkcie dla danej tekstury *)
let compute_texture ent color_tex col_pos =
	match color_tex with
	Empty_texture				-> vec 1. 1. 1.
	| Standard_texture(tex,u,v)	-> Texture.sample tex (entity_texcoord ent col_pos u v)
;;

let rec ray_trace r ents lights max_refl =
	let r_orig,r_dir = r in

	match closest_object r ents with
	No_collision				-> vec 0. 0. 0.
	| Collision(dist,ent,mat)	->
		let col_pos = r_orig +| (r_dir *| dist) in
		let mat_color,tex1,tex2,refl_val = mat in
		let nrm = compute_normal ent tex2 col_pos in
        let col_mult = mulv (compute_texture ent tex1 col_pos) mat_color in
		
		let add_light last_color (pos,tcol,vol,dens) =
			let last_ent = ref Dummy in
			let fulldens = (float_of_int (dens.(0))) *. (float_of_int (dens.(1))) *. (float_of_int (dens.(2))) in
			let col = tcol *| (0.1/.fulldens) in

			let compute_color pos =
				let to_light= unitize pos in
				let new_ray = (col_pos+|(to_light*|0.000001),to_light) in
				let r2l_dist = length (pos -| col_pos) in
				
				if( match !last_ent with Dummy -> false | _ -> (ray_ent new_ray !last_ent)<r2l_dist )
				then vec 0. 0. 0.
				else if( match closest_object new_ray ents with
						No_collision			-> last_ent := Dummy; true
						| Collision(dist,obj,_)	-> last_ent := obj; dist>r2l_dist )
						then ( ( (mulv col col_mult) *| (clamp (dot nrm to_light) 0. 1.)
								*| (1. /. length_sq(pos -| col_pos)) ) +| last_color  )
						else last_color
			in
			let mulvec = [| (vol.(0))/.(float_of_int (dens.(0)));
							(vol.(1))/.(float_of_int (dens.(1)));
							(vol.(2))/.(float_of_int (dens.(2))) |] in
			let out_color = ref last_color in

			let addPos = pos -| (mulv mulvec [| (float_of_int (dens.(0)))*.0.5; (float_of_int (dens.(1)))*.0.5; (float_of_int (dens.(2)))*.0.5; |]) in
			
			for x = 0 to (dens.(0))-1 do
				for y = 0 to (dens.(1))-1 do
					for z = 0 to (dens.(2))-1 do
						let tpos = addPos +| (mulv mulvec [| (float_of_int x); (float_of_int y); (float_of_int z) |]) in
						out_color := (compute_color tpos) +| !out_color
					done;
				done;
			done;
			!out_color
		in
		let out = List.fold_left add_light (vec 0. 0. 0.) lights in
		
		if refl_val > 0.000001
		then
			let reflection = if max_refl=0
				then vec 0. 0. 0.
				else let d=unitize (reflect r_dir nrm) in
					ray_trace (r_orig+|(d*|0.000001),d) ents lights (max_refl-1)
			in
			out *| (1.-.refl_val) +| (reflection*|refl_val)
		else
			out
;;

