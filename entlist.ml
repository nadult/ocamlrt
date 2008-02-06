open Base;;

let entities_bbox entities =
    if List.length entities < 1 then (vec 0. 0. 0.),(vec 0. 0. 0.) else

    let update_bbox (bmin1,bmax1) (_,_,_,(bmin2,bmax2)) =
        (vmin bmin1 bmin2),(vmax bmax1 bmax2)
    in
    let (_,_,_,(tmin,tmax)) = List.hd entities in

    List.fold_left update_bbox (tmin,tmax) entities
;;

let create entities =

    let ray_list (r:ray_t) (td:tracedata_t) = (
        let ray_ent c1 (e:entity_t) =
            let (f,_,_,_) = e in
            let c2 = f r td in
            match c1 with
            No_collision        -> (
                match c2 with
                No_collision        -> No_collision
                | Collision(d2,e2)  -> Collision(d2,extract_entityref e e2)  )
            | Collision(d1,e1)  -> (
                match c2 with
                No_collision        -> c1
                | Collision(d2,e2)  -> if d1<d2 then c1 else Collision(d2,extract_entityref e e2)  )
        in
        List.fold_left ray_ent No_collision entities
    ) in

    let get_normal _ = vec 0. 0. 0. in
    let get_color _ = vec 0. 0. 0., 0. in
    let bmin,bmax = entities_bbox entities in

    ( (ray_list, get_normal, get_color, (bmin,bmax)) : entity_t )
;;
