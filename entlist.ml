
open Base;;
open Scene;;


let make_entlist entities =

    let ray_list r = (
        let ray_ent c1 (f,_,_,_) =
            let c2 = f r in
            match c1 with
            No_collision        -> c2
            | Collision(d1,e1)  ->
                ( match c2 with
                No_collision        -> c1
                | Collision(d2,e2)  -> if d1<d2 then c1 else c2 )
        in
        List.fold_left ray_ent No_collision entities
    ) in

    let update_bbox (bmin1,bmax1) (_,_,_,(bmin2,bmax2)) =
        (minv bmin1 bmin2),(maxv bmax1 bmax2)
    in
    let (_,_,_,(tmin,tmax)) = List.hd entities in
    let bmin,bmax = List.fold_left update_bbox (tmin,tmax) entities in

    let get_normal _ = vec 0. 0. 0. in
    let get_color _ = vec 0. 0. 0. in

    (ray_list, get_normal, get_color, (bmin,bmax))
;;
