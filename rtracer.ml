open Base;;
open Scene;;
open Printf;;

let rec ray_trace r ents lights max_refl =
	let r_orig,r_dir = r in
    let r_idir = vinv r_dir in

    let (ray_ents,_,_,_) = ents in

    match ray_ents r (r_idir,0.,10000.) with
    No_collision                        -> vec 0. 0. 0.
    | Collision(dist,Entityref(ent))    -> (
        let (_,get_nrm,get_color,_) = ent in
        let col_pos = r_orig +| (r_dir *| dist) in
        let (color,refl),normal = get_color col_pos,get_nrm col_pos in

        let refl_color = (
            if refl>0.0001 && max_refl > 0
            then
                let d = vunit ( vreflect r_dir normal )  in
                ray_trace (col_pos+|(d*|0.000001),d) ents lights (max_refl-1)
            else vec 0.0 0.0 0.0
        ) in

        let add_light last_color (lpos,lcol) =
            let lightDist = length ( lpos -| col_pos ) in
            let to_light = vunit (lpos -| col_pos ) in
            let zero = vec 0. 0. 0. in

            let new_ray = (col_pos+|(to_light*|0.00001),to_light) in
            let new_rayidir = vinv to_light in
            let shadowed = (
                match ray_ents new_ray (new_rayidir,0.,10000.) with
                No_collision                        -> false
                | Collision(dist,Entityref(ent2))   -> (dist < lightDist) ) in

            if shadowed
            then last_color
            else (
                let light_value = lcol *| ( (vdot to_light normal) /. (lightDist*.lightDist) ) in
                last_color +| vmax zero light_value
            )
        in
        let light_val = List.fold_left add_light (vec 0. 0. 0.) lights in

        vmul ( (color *| (1.0-.refl)) +| (refl_color *| refl) )
                light_val
    )
;;

