open Base;;

let create p1 p2 p3 (mat:material_t) =

    let bmin,bmax = (vmin (vmin p1 p2) p3), (vmax (vmax p1 p2) p3) in
    let nrm = vunit (vcross (p2 -| p1) (p3 -| p1) ) in
    let d = vdot nrm p1 in

    let sample_color,sample_normal = mat in

    let ray_triangle (r:ray_t) (_:tracedata_t) =
        let r_orig,r_dir = r in
        let cosa = vdot r_dir nrm in

       (* if (if cosa < 0. then -.cosa else cosa) < 0.000001
        then
            infinity
        else *)
            let dist = (d -. (vdot r_orig nrm)) /. cosa in

	        let r_orig,r_dir = r in
		    let hit=r_orig+|(r_dir*|dist) in
            let side1 = vdot (vcross (p2-|p1) nrm) (hit-|p1) in
            let side2 = vdot (vcross (p3-|p2) nrm) (hit-|p2) in
            let side3 = vdot (vcross (p1-|p3) nrm) (hit-|p3) in
    		if side1<0.&&side2<0.&&side3<0.
                then create_collision dist
                else No_collision
    in

    let compute_uv col =
        vec (vx col) ((vy col)+.(vz col)) 0.
    in

    let get_nrm col =
        sample_normal (compute_uv col) nrm
    in

    let get_color col =
        sample_color (compute_uv col)
    in

   (  ( ray_triangle, get_nrm, get_color, (bmin,bmax) ) : entity_t )
;;

