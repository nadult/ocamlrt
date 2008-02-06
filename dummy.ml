open Base;;

let create (_:unit) =

    let ray_dummy (_:ray_t) (_:tracedata_t) =
        No_collision
    in

    let get_nrm (_:float array) =
        vec 0. 0. 0.
    in

    let get_color (_:float array) =
        vec 0. 0. 0., 0.
    in

   (  ( ray_dummy, get_nrm, get_color, ((vec 0. 0. 0.),(vec 0. 0. 0.)) ) : entity_t )
;;

