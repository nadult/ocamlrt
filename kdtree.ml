(* Definicje obiektów sceny + funkcje ³aduj¹ce *)

open Base;;
open Scene;;


(* Na wejsciu dostaje liste par ent,mat (skonczonych) *)
let rec make_kdtree objects axis deep =
    if ((List.length objects) < 4) || deep>5
    then
        Entlist.make_entlist objects
    else
    	let sorted = List.sort
            (fun (_,_,_,(min1,_)) (_,_,_,(min2,_)) -> int_of_float ( ((min1).(axis)) -. (min2.(axis)) ) )
            objects
        in
	    let min,max = Entlist.entities_bbox objects in
    	let pos = ((min +| max) *| 0.5).(axis) in

        let leftSide =  List.fold_left
            (fun lst e -> let _,_,_,(min,_)=e in if (min.(axis)) < pos then e::lst else lst) [] sorted in
        let rightSide = List.fold_left
            (fun lst e -> let _,_,_,(_,max)=e in if (max.(axis)) > pos then e::lst else lst) [] sorted in

        let leftk = make_kdtree leftSide ((axis+1) mod 3) (deep+1) in
        let rightk = make_kdtree rightSide ((axis+1) mod 3) (deep+1) in

        let ray_kdnode r td =
            let (r_orig,r_dir),(r_idir,min,max) = r,td in
            let orig,dir,idir = (r_orig.(axis)),(r_dir.(axis)),(r_idir.(axis)) in
            let tpos = (pos-.orig)*.idir in
            
            if max<tpos then
                let (f,_,_,_) = leftk  in f r (r_idir,min,tpos)
            else if min>tpos then
                let (f,_,_,_) = rightk in f r (r_idir,tpos,max)
            else (
                let (f1,_,_,_),(f2,_,_,_) = leftk,rightk in
                let c1,c2 = (f1 r (r_idir,min,tpos)),(f2 r (r_idir,tpos,max)) in
                match c1 with
                No_collision        -> c2
                | Collision(d1,e1)  -> (
                    match c2 with
                    No_collision        -> c1
                    | Collision(d2,e2)  -> if d1<d2 then c1 else c2 )
            )
        in

        let get_normal _ = vec 0. 0. 0. in
        let get_color _ = vec 0. 0. 0. in

        (ray_kdnode, get_normal, get_color, (min,max))
;;

