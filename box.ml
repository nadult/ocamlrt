open Base;;

let create p1 p2 (mat:material_t) =

    let sample_color,sample_normal = mat in

    let ray_box (r:ray_t) ((idir,_,_):tracedata_t) =
        let r_orig,r_dir = r in

        let compute_minmax axis =
            let min = (p1.(axis)-.r_orig.(axis))*.idir.(axis) in
            let max = (p2.(axis)-.r_orig.(axis))*.idir.(axis) in
            if min<max then min,max else max,min
        in

        let xmin,xmax = compute_minmax 0 in
        let ymin,ymax = compute_minmax 1 in

        if ymin>xmax || xmin>ymax
        then No_collision
        else (
            let min = if xmin>ymin then xmin else ymin in
            let max = if xmax<ymax then xmax else ymax in

            let zmin,zmax = compute_minmax 2 in

            if zmin>max || min>zmax
            then No_collision
            else create_collision (if zmin>min then zmin else min)
        )
    in
    
    let mid = (p1+|p2)*|0.5 in
    let isize = vinv (p1-|p2) in

    let compute_uv col =
        let p = vmul (col-|mid) isize in

        vec ((vx p)+.(vy p)*.0.5) (vz p) 0.
    in

    let get_nrm col =
        let d = vmul (mid -| col) isize in
        let ad = vabs d in

        let nrm =
            if vx ad > vy ad
            then ( if vz ad > vx ad
                then ( if vz d<0. then vec 0. 0. (-.1.) else vec 0. 0. 1. )
                else ( if vx d<0. then vec (-.1.) 0. 0. else vec 1. 0. 0. ) )
            else ( if vz ad > vy ad
                then ( if vz d<0. then vec 0. 0. (-.1.) else vec 0. 0. 1. )
                else ( if vy d<0. then vec 0. (-.1.) 0. else vec 0. 1. 0. ) )
         in

        sample_normal (compute_uv col) nrm
    in

    let get_color col =
        sample_color (compute_uv col)
    in

   (  ( ray_box, get_nrm, get_color, (p1,p2) ) : entity_t )
;;

