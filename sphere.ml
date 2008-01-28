open Base;;

let create pos rad mat =
    let radp2,irad2,irad = rad *. rad, 0.5 /. rad, 1.0 /. rad in

    let ray_sphere r _ =
	    let r_orig,r_dir = r in
	
    	let dst = r_orig -| pos in
    	let b = dot dst r_dir in
	    let c = (dot dst dst) -. radp2 in
    	let d = b *. b -. c in
	    if d > 0.
        then Collision( -. b -. (sqrt d), Entityref_this )
        else No_collision
    in

    let sample_color, sample_normal = mat in

    let compute_uv col =
        [| asin((((col.(0)-.pos.(0))+.(col.(2)-.pos.(2))))*.irad2)*.2. ;
        asin( (col.(1)-.pos.(1))*.irad ) |]
    in

    let get_normal col =
        sample_normal (compute_uv col) (unitize (col -| pos))
    in

    let get_color col =
        sample_color (compute_uv col)
    in

    let rv = vec rad rad rad in
    let bmin,bmax = pos -| rv, pos +| rv in

    ( ( ray_sphere, get_normal, get_color, (bmin,bmax) ) : entity_t )

;;
