OCAMLC=ocamlopt
FLAGS=-ccopt -O3
FILES=base.ml texture.ml box.ml material.ml dummy.ml triangle.ml sphere.ml plane.ml entlist.ml kdtree.ml \
	generators.ml scene.ml rtracer.ml main.ml

all: rtracer
	
rtracer: $(FILES)
	$(OCAMLC) $(FLAGS) /usr/lib/ocaml/3.09.2/graphics.cmxa $(FILES) -o rtracer

clean:
	rm *.cmx *.cmi *.cmo *.o rtracer


