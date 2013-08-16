:- module(
  iotw,
  [
    rdf_shared/1, % +Graph:atom
    rdf_shared/2, % +Graph:atom
                  % +Alignments:list(pair)
    rdf_shared/3, % +Graph:atom
                  % +Alignments:list(pair)
                  % -SVG:dom
% SUPPORT PREDICATES
    same_predicate/2,
    same_object/2
  ]
).

/** <module> IOTW

Coordinates the Itentity on the Web experiment.

# Identity & equality

## Non-equal identicals

In the value space of XSD floats, NaN is identical with itself, but
not equivalent with itself.

## Non-identical equals

In the value space of XSD floats, -0 and 0 are equal but not identical.

@author Wouter Beek
@version 2013/04-2013/05, 2013/08
*/

:- use_module(gv(gv_file)).
:- use_module(iotw(iimb)).
:- use_module(iotw(iotw_pairs)).
:- use_module(iotw(iotw_pairs_export)).
:- use_module(os(run_ext)).
%:- use_module(rdf(rdf_graph)).

:- rdf_meta(same_object(r,r)).
:- rdf_meta(same_predicate(r,r)).

%:- initialization(run_experiment).



%! rdf_shared(+Graph:atom) is det.
% Establish the pairs of shared properties for the given graph.
%
% @see rdf_shared/2

rdf_shared(Graph):-
  rdf_shared(Graph, []).

%! rdf_shared(+Graph:atom, +Alignments:list(pair)) is det.
% Establish the pairs of shared properties for the given graph.
% Also annotate the alignment overlap per pair cluster.
%
% Writes the results to a file with the given graph name.

rdf_shared(RDF_Graph, Alignments):-
  rdf_shared_pairs(RDF_Graph, Tuples),
  export_rdf_shared(RDF_Graph, Alignments, Tuples, GIF),
  graph_to_gv_file([], GIF, dot, pdf, PDF_File),
  open_pdf(PDF_File). %DEB

%! rdf_shared(+Graph:atom, +Alignments:list(pair), -SVG:dom) is det.
% Returns the SVG representation of the shared properties between pairs
% in the given graph, plus an annotation of the alignment overlap.

rdf_shared(RDF_Graph, Alignments, SVG):-
  rdf_shared_pairs(RDF_Graph, Tuples),
  export_rdf_shared(RDF_Graph, Alignments, Tuples, GIF),
  graph_to_svg_dom([], GIF, dot, SVG).

run_experiment:-
  load_alignment_iimb(1, SVG),
  write(SVG).

same_object(O, O).

same_predicate(P, P).

