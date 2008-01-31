(* G³ówne procedurki licz¹ce *)

open Base;;
open Scene;;
open Printf;;

let rec ray_trace r ents lights max_refl =
	let r_orig,r_dir = r in
    let r_idir = vec (1.0/.(r_dir.(0))) (1.0/.(r_dir.(1))) (1.0/.(r_dir.(2))) in

    let (ray_ents,_,_,_) = ents in

    match ray_ents r (r_idir,0.,10000.) with
    No_collision                        -> vec 0. 0. 0.
    | Collision(dist,Entityref(ent))    -> (
        let (_,get_nrm,get_color,_) = ent in
        let col_pos = r_orig +| (r_dir *| dist) in
        let color,normal = get_color col_pos,get_nrm col_pos in

        let add_light last_color (lpos,lcol,lvol,ldens) =
            let lightDist = length ( lpos -| col_pos ) in
            let to_light = unitize (lpos -| col_pos ) in

            let new_ray = (col_pos+|(to_light*|0.00001),to_light) in
            let new_rayidir = vec (1.0/.(to_light.(0))) (1.0/.(to_light.(1))) (1.0/.(to_light.(2))) in
            let shadowed = (
                match ray_ents new_ray (new_rayidir,0.,10000.) with
                No_collision                        -> false
                | Collision(dist,Entityref(ent2))   -> (dist < lightDist) ) in

            if shadowed
            then last_color
            else (
                let light_value = lcol *| ( (dot to_light normal) /. (lightDist*.lightDist) ) in
                last_color +| light_value
            )
        in
        let light_val = List.fold_left add_light (vec 0. 0. 0.) lights in

        mulv color light_val
    )


    (*

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

        *)
;;

