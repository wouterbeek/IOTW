:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).

run:-
  rdf_assert(ex:a, ex:p, literal(type(xsd:string,b))),
  rdf_assert(ex:a, owl:sameAs, literal(type(xsd:string,b))),
  rdf_print_graph(default),
  true.
