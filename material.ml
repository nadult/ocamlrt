open Base;;

(* !!Dorobic odblaski
 * Zwraca obiekt typu material_t
 * *)
let create (color:float array) color_tex normal_tex refl =

    let sample_color = (
        match color_tex with
        Standard_texture(t,mu,mv) ->
            ( fun uv -> mulv (   Texture.sample t ( (mu*.(uv.(0))),(mv*.(uv.(1))) )   ) color )
        | Empty_texture           ->
            ( fun _ -> color ) 
    ) in

    let sample_normal = (
        match normal_tex with
        Standard_texture(t,mu,mv) ->
            ( fun uv base_nrm ->
                let tex_val =  Texture.sample t ( (mu*.(uv.(0))),(mv*.(uv.(1))) ) in
		        let tex_nrm = (tex_val *| 2.) -| (vec 1. 1. 1.) in
        		let tg1 = vec (-.(base_nrm.(1))) (base_nrm.(0)) (base_nrm.(2)) in let tg2=cross base_nrm tg1 in
        		transform tex_nrm (transpose (vec tg1 tg2 base_nrm)) )
        | Empty_texture           ->
           ( fun _ nrm -> nrm )
    ) in

    ( ( sample_color, sample_normal ) : material_t )
;;

let empty =
    create (vec 1. 1. 1.) Empty_texture Empty_texture 0.
;;

