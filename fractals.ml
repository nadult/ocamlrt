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

