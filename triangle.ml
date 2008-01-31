open Base;;

let create p1 p2 p3 (mat:material_t) =

    let bmin,bmax = (minv (minv p1 p2) p3), (maxv (maxv p1 p2) p3) in
    let nrm = unitize (cross (p2 -| p1) (p3 -| p1) ) in
    let d = dot nrm p1 in

    let sample_color,sample_normal = mat in

    let ray_triangle (r:ray_t) (_:tracedata_t) =
        let r_orig,r_dir = r in
        let cosa = dot r_dir nrm in

       (* if (if cosa < 0. then -.cosa else cosa) < 0.000001
        then
            infinity
        else *)
            let dist = (d -. (dot r_orig nrm)) /. cosa in

	        let r_orig,r_dir = r in
		    let hit=r_orig+|(r_dir*|dist) in
            let side1 = dot (cross (p2-|p1) nrm) (hit-|p1) in
            let side2 = dot (cross (p3-|p2) nrm) (hit-|p2) in
            let side3 = dot (cross (p1-|p3) nrm) (hit-|p3) in
    		if side1<0.&&side2<0.&&side3<0.
                then create_collision dist
                else No_collision
    in

    let compute_uv col =
        [| (col.(0)); ((col.(1))+.col.(2)) |]
    in

    let get_nrm col =
        sample_normal (compute_uv col) nrm
    in

    let get_color col =
        sample_color (compute_uv col)
    in

   (  ( ray_triangle, get_nrm, get_color, (bmin,bmax) ) : entity_t )
;;

