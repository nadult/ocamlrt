(* Definicje obiektów sceny + funkcje ³aduj¹ce *)

open Base;;
open Scanf;;
open Printf;;


let create_sphereflake start_pos start_size start_level material =
    let rec inner lst level pos size2 =
        if level=0 then lst else

        let t = (Sphere.create pos size2 material) :: lst in let size = size2/.2. in
        let p = 0.433012702*.1.5*.size2 in let n,z = -.p,0. in

        let cr x y z p = inner p (level-1) [| pos.(0)+.x; pos.(1)+.y; pos.(2)+.z; |] size in
        if (level mod 2)=0
        then (cr z p p (cr n p n (cr p p n (cr z n n (cr n n p (cr p n p t ))))))
        else (cr z n p (cr n n n (cr p n n (cr z p n (cr n p p (cr p p p t ))))))
    in
    inner [] start_level start_pos start_size
;;

let create_boxlight pos size density color =
    let dx,dy,dz = density in
    let mul = vmul ( size*|0.5 ) (vinv (vec (float_of_int dx) (float_of_int dy) (float_of_int dz))) in
    let start_pos = pos-|(size*|0.5) in
    let tcolor = color *| (1.0 /. (float_of_int (dx*dy*dz))) in

    let rec gen_lights x y z =

        let light = (
            let tpos = vec (float_of_int x) (float_of_int y) (float_of_int z) in
            let pos = start_pos+| (vmul tpos mul) in
            ( pos, color )
        ) in

             if x<dx then light :: (gen_lights (x+1) y z)
        else if y<dy then light :: (gen_lights 1 (y+1) z)
        else if z<dz then light :: (gen_lights 1 1 (z+1))
        else [ light ]
    in

    gen_lights 1 1 1
;;
