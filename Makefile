OCAMLC=ocamlopt
FLAGS=-cc g++-4.2 -ccopt -O3
FILES=base.ml texture.ml scene.ml rtracer.ml main.ml triangle.ml material.ml sphere.ml plane.ml entlist.ml kdtree.ml

all: rtracer
	
rtracer: $(FILES)
	$(OCAMLC) $(FLAGS) /usr/lib/ocaml/3.09.2/graphics.cmxa $(FILES) -o rtracer

clean:
	rm *.cmx *.cmi *.cmo *.o

