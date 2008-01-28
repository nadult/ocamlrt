open Base;;
open Scanf;;
open Printf;;

let input_short input =
	let h,l = input_byte input,input_byte input in
	l+h*256
;;

let load tex_name = 
	let input = open_in_bin (tex_name^".tga") in
	
	for t=1 to 12 do input_byte input done;
	let resx,resy=input_short input,input_short input in
	for t=1 to 2 do input_byte input done;
	
	let arr=Array.make (resx*resy) [|0.;0.;0.|] in
	
	for y=0 to resy-1 do
		for x=0 to resx-1 do
			let r,g,b=input_byte input,input_byte input,input_byte input in
			arr.(x+y*resx) <- [| (float_of_int r)/.255.; (float_of_int g)/.255.; (float_of_int b)/.255. |];
		done
	done;
	(resx, resy, arr, tex_name)
;;

(* Laduje teksturki z wejscia;
	na wyjsciu liste tekstur *)
let load_all (_:unit) =
	parse_list "textures" ([]: texture_t list)
		(fun lst ->
			let name = scanf "%s" id in
			(load name) :: lst
		)
;;

let sample tex (u,v) =
	let u,v=u+.10000.,v+.10000. in
	let tu,tv=u-.(float_of_int (int_of_float u)),v-.(float_of_int (int_of_float v)) in
	let resx,resy,arr,_=tex in
	let px,py=(float_of_int resx)*.tu,(float_of_int resy)*.tv in
	let ix,iy=clamp (int_of_float px) 0 (resx-1),clamp (int_of_float py) 0 (resy-1) in
	let dx,dy=px-.(float_of_int ix),py-.(float_of_int iy) in
	
	let r1=
		(	(arr.(ix+iy*resx))					*|(1.-.dx)	)+|
		(	(arr.(((ix+1) mod resx)+iy*resx))	*|dx 		) in
	let r2=
		(	(arr.(ix+((iy+1) mod resy)*resx))					*|(1.-.dx)	)+|
		(	(arr.(((ix+1) mod resx)+((iy+1) mod resy)*resx))	*|dx 		) in
	
	(r1*|(1.-.dy))+|r2*|dy
;;
