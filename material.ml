open Base;;

(* !!Dorobic odblaski
 * Zwraca obiekt typu material_t
 * *)
let create (color:float array) color_tex normal_tex refl =

    let sample_color = (
        match color_tex with
        Standard_texture(t,mu,mv) ->
            ( fun uv -> (vmul (   Texture.sample t ( (mu*.(vx uv)),(mv*.(vy uv)) )   ) color), refl )
        | Empty_texture           ->
            ( fun _ -> color, refl ) 
    ) in

    let sample_normal = (
        match normal_tex with
        Standard_texture(t,mu,mv) ->
            ( fun uv base_nrm ->
                let tex_val =  Texture.sample t ( (mu*.(vx uv)),(mv*.(vy uv)) ) in
		        let tex_nrm = (tex_val *| 2.) -| (vec 1. 1. 1.) in
        		let tg1 = vec (-.(vy base_nrm)) (vx base_nrm) (vz base_nrm) in let tg2=vcross base_nrm tg1 in
        		transform tex_nrm (transpose (vec tg1 tg2 base_nrm)) )
        | Empty_texture           ->
           ( fun _ nrm -> nrm )
    ) in

    ( ( sample_color, sample_normal ) : material_t )
;;

let empty =
    create (vec 1. 1. 1.) Empty_texture Empty_texture 0.
;;

