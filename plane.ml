open Base;;

let create nrm dist mat =

    let ray_plane r _ =
        let r_orig,r_dir = r in
        let cosa = vdot r_dir nrm in
        if (if cosa < 0. then -.cosa else cosa) < 0.000001
        then No_collision
        else create_collision ((dist -. (vdot r_orig nrm)) /. cosa)
    in

    let sample_color, sample_normal = mat in

    let compute_uv col =
        [| vx col; (vy col)+.(vz col) |]
    in

    let get_normal col =
        sample_normal (compute_uv col) (nrm)
    in

    let get_color col =
        sample_color (compute_uv col)
    in

    let bmin = (vec (-.infinity) (-.infinity) (-.infinity) ) and
        bmax = (vec infinity infinity infinity ) in

    ( ( ray_plane, get_normal, get_color, (bmin,bmax) ) : entity_t )
;;
