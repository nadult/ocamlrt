open Base;;


(* Parametry do algorytmu SAH *)
let split_cost = 1.0;;
let intersect_cost = 0.45;;


let rec create_ objects deep bmin bmax=
    if List.length objects < 1
    then Dummy.create ()
    else (
        let setvn vect axis nval =
            if axis=0 then vec nval (vect.(1)) (vect.(2))
            else if axis=1 then vec (vect.(0)) nval (vect.(2))
            else vec (vect.(0)) (vect.(1)) nval
        in

        (* Wylicza koszt podzialu *)
        let eval_cost axis pos =
            let ns,pos_in_node = bmax-|bmin,(pos-.bmin.(axis))/.(bmax.(axis)-.bmin.(axis)) in

            (* Wylicza powierzchnie pudelka o danych wymiarach *)
            let eval_surf s = (vx s)*.(vy s)+.(vx s)*.(vz s)+.(vy s)*.(vz s) in


            let left_cost  = eval_surf (setvn ns axis (ns.(axis)*.pos_in_node)       ) in
            let right_cost = eval_surf (setvn ns axis (ns.(axis)*.(1.-.pos_in_node)) ) in

            (* Ilosc obiektow w lewym, prawym wezle *)
            let l,r = List.fold_left ( fun (l,r) (_,_,_,(mi,ma))      -> 
                (if mi.(axis)<pos then l+1 else l),(if ma.(axis)>pos then r+1 else r) )
                (0,0) objects in

            split_cost +.
                ( left_cost *. (float_of_int l) +. right_cost *. (float_of_int r) )
                    /. (eval_surf ns) *. intersect_cost
        in

        (* Algorytm SAH (da sie w czasie n log n, ale jakos mi sie nie chce :)
         * Zwraca pare (koszt,pozycja)
         * *)
        let find_optimal_split axis =
            List.fold_left (fun (s_cost,s_pos) (_,_,_,(min,max)) ->
                let c1,c2 = eval_cost axis min.(axis),eval_cost axis max.(axis) in
                if c1 < s_cost
                then ( if c2 < c1     then c2,max.(axis) else c1,min.(axis) )
                else ( if c2 < s_cost then c2,max.(axis) else s_cost,s_pos  ) )
                (infinity,bmin.(axis)) objects 
        in            

        let (c0,p0),(c1,p1),(c2,p2) = find_optimal_split 0,find_optimal_split 1,find_optimal_split 2 in

        let cost,pos,axis = if c1 < c0
            then (if c2 < c1 then c2,p2,2 else c1,p1,1)
            else (if c2 < c0 then c2,p2,2 else c0,p0,0)
        in
       
        (*
        let axis = deep mod 3 in
        let pos = ((bmax+|bmin)*|0.5).(axis) in
        let cost = eval_cost axis pos in
        *)

        if cost > intersect_cost*.(float_of_int (List.length objects))
        then (* Nie oplaca sie dalej dzielic *)
            Entlist.create objects
        else (

            let leftSide =  List.fold_left
                (fun lst e -> let _,_,_,(min,_)=e in if (min.(axis)) < pos then e::lst else lst) [] objects in
            let rightSide = List.fold_left
                (fun lst e -> let _,_,_,(_,max)=e in if (max.(axis)) > pos then e::lst else lst) [] objects in

            let leftk = create_ leftSide (deep+1) bmin (setvn bmax axis pos) in
            let rightk = create_ rightSide (deep+1) (setvn bmin axis pos) bmax in

            let ray_kdnode r td =
                let (r_orig,r_dir),(r_idir,min,max) = r,td in
                let orig,dir,idir = (r_orig.(axis)),(r_dir.(axis)),(r_idir.(axis)) in
                let tpos = (pos-.orig)*.idir in

                let close,far = if idir>=0. then (leftk,rightk) else (rightk,leftk) in
            
                if max<tpos then
                    let (f,_,_,_) = close in f r (r_idir,min,tpos)
                else if min>tpos then
                    let (f,_,_,_) = far   in f r (r_idir,tpos,max)
                else (
                    let (f1,_,_,_),(f2,_,_,_) = close,far in
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
            let get_color _ = vec 0. 0. 0., 0. in
   
            ( (ray_kdnode, get_normal, get_color, (bmin,bmax)) : entity_t )
        )
    )
;;

let create objects =
    let bmin,bmax = Entlist.entities_bbox objects in
    create_ objects 0 bmin bmax;;

