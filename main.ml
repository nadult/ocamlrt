open Base;;
open Scanf;;
open Printf;;

let iterate_pixels (resx,resy) func =
	for y = 0 to resy-1 do
		for x = 0 to resx-1 do
			func x y
		done;
	done;
;;

let gen_picture (resx,resy) entities lights max_refl screen_plane_dist (sy,ny) =
	let arr = Array.make (resx*ny) 0 in

	for y = sy to sy+ny-1 do
		for x = 0 to resx-1 do
			let r = ( vec 0. 0. 0.,unitize (vec 
				(((float_of_int x) /. (float_of_int resx) -. 0.5) *. ((float_of_int resx) /. (float_of_int resy)))
				((float_of_int y) /. (float_of_int resy) -. 0.5)
				screen_plane_dist) ) in
				
			let color = Rtracer.ray_trace r entities lights max_refl in
			arr.(x+(y-sy)*resx) <- vec2color color
		done;
		printf "%d " y;
		flush stdout;
	done;
	
	(resx,resy,arr)
;;

let draw_picture pic =
	let resx,resy,arr = pic in
	Graphics.open_graph ((string_of_int resx)^"x"^(string_of_int resy));
	Graphics.auto_synchronize false;
	
	iterate_pixels (resx,resy) (fun x y->
			let rgb = arr.(x+y*resx) in
			Graphics.set_color rgb;
			Graphics.plot x y	);
	(* Graphics.blit_image (Graphics.make_image
			( let out = Array.make resy (Array.make 1 0) in
				for y = 0 to resy-1 do
					let line = Array.make resx 0 in
					for x = 0 to resx-1 do line.(x) <- arr.(x+y*resx) done;
					out.(y) <- line
				done; out
			)
	) 0 (resy-1); *)
	Graphics.synchronize();
	let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
	Graphics.close_graph
;;

(*
 * Od razu generuje obrazek do pliku
 *)
let gen_tga file_name (resx,resy) entities lights max_refl screen_plane_dist =
	let out=open_out_bin file_name in

	printf "\nWriting to %s ..." file_name; flush stdout;

        List.fold_left (fun a b -> output_byte out b) ()
            [0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0];

        output_byte out (resx mod 256);
        output_byte out (resx / 256);
        output_byte out (resy mod 256);
        output_byte out (resy / 256);

        output_byte out 24;
        output_byte out 0;
	
    for y=0 to resy-1 do
        let _,_,arr = gen_picture (resx,resy) entities lights max_refl screen_plane_dist (y,1) in
        for x=0 to resx-1 do
			let rgb = arr.(x) in
			output_byte out (rgb /(256*256));
			output_byte out ((rgb /256)mod 256);
			output_byte out (rgb mod 256);
        done;
    done;
			
	printf "...done\n";
;;

(*
 * Zapisuje obrazek do pliku
 *)
let print_tga pic file_name =
	let resx,resy,arr = pic in
	let out=open_out_bin file_name in

	printf "\nWriting to %s ..." file_name; flush stdout;

        List.fold_left (fun a b -> output_byte out b) ()
            [0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0];

        output_byte out (resx mod 256);
        output_byte out (resx / 256);
        output_byte out (resy mod 256);
        output_byte out (resy / 256);

        output_byte out 24;
        output_byte out 0;
	
	iterate_pixels (resx,resy) (fun x y->
			let rgb = arr.(x+(resy-y-1)*resx) in
			output_byte out (rgb /(256*256));
			output_byte out ((rgb /256)mod 256);
			output_byte out (rgb mod 256) );
			
	printf "...done\n";
;;

let main (_:unit) =
	let resx,resy,max_refl,screen_plane_dist = scanf "(%d,%d) %d %f\n" (fun a b c d -> (a,b,c,d)) in
	(*  *)
	
	let textures = Texture.load_all() in
    let materials = Scene.load_materials textures in
	let entities = Scene.load_entities materials in
	let lights = Scene.load_lights() in

   (*  let sfmat = ( [| 1.;1.;1. |], Scene.Empty_texture, Scene.Empty_texture, 0.) in
     let sphereflake = Scene.gen_sphereflake [|-3.;0.;35.|] 9. 6 sfmat in *)

    gen_tga "output.tga" (resx,resy) entities lights max_refl screen_plane_dist;
;;

main ()
