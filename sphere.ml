open Base;;

let create pos rad mat =
    let radp2,irad2,irad = rad *. rad, 0.5 /. rad, 1.0 /. rad in

    let ray_sphere r _ =
	    let r_orig,r_dir = r in
	
    	let dst = r_orig -| pos in
    	let b = vdot dst r_dir in
	    let c = (vdot dst dst) -. radp2 in
    	let d = b *. b -. c in
	    if d > 0.
        then create_collision (-. b -. (sqrt d))
        else No_collision
    in

    let sample_color, sample_normal = mat in

    let compute_uv col =
        let diff = pos -| col in
        vec (asin(((vx diff)+.(vz diff))*.irad2)*.2.) (asin((vy diff)*.irad)) 0.
    in

    let get_normal col =
        sample_normal (compute_uv col) (vunit (col -| pos))
    in

    let get_color col =
        sample_color (compute_uv col)
    in

    let rv = vec rad rad rad in
    let bmin,bmax = pos -| rv, pos +| rv in

    ( ( ray_sphere, get_normal, get_color, (bmin,bmax) ) : entity_t )

;;
