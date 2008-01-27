open Base;;
open Scene;;

let make_plane nrm dist mat =

    let ray_plane r =
        let r_orig,r_dir = r in
        let cosa = dot r_dir nrm in
        if (if cosa < 0. then -.cosa else cosa) < 0.000001
        then No_collision
        else Collision( (dist -. (dot r_orig nrm)) /. cosa, Entityref_this )
    in

    let sample_color, sample_normal = mat in

    let compute_uv col =
        [| (col.(0)) ; (col.(1)) |]
    in

    let get_normal col =
        sample_normal (compute_uv col) (nrm)
    in

    let get_color col =
        sample_color (compute_uv col)
    in

    let bmin = (vec (-.infinity) (-.infinity) (-.infinity) ) and
        bmax = (vec infinity infinity infinity ) in

    ( ray_plane, get_normal, get_color, (bmin,bmax) )
;;
