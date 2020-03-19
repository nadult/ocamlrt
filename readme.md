Simple Whitted-style raytracer written in OCaml.

## Features

- texture & normal mapping
- KD tree with SAH (Surface area heuristic) method used for building
- reflections
- soft shadows
- Primitive support: plane, sphere, triangle, box

## How to use

Compile with make
To run, pass scene description to stdin:
./rtracer < input
output.tga will be generated.

Syntax of input file is very strict, all spaces and tabs have to be in their appropriate places.

## Example images

![](https://nadult.github.io/images/ocaml_raytracer/1.jpg "Picture #1")
  
![](https://nadult.github.io/images/ocaml_raytracer/2.jpg "Picture #2")
  
![](https://nadult.github.io/images/ocaml_raytracer/3.jpg "Picture #3")
