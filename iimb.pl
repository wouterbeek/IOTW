:- module(
  iimb,
  [
    load_alignment_iimb/2 % +Number:nonneg
                          % -SVG:dom
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

--

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(iotw(iotw_alignments)).
:- use_module(iotw(iotw_alignments_export)).
:- use_module(library(debug)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(
  'IIMBTBOX',
  'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
).
:- db_add_novel(user:file_search_path(iimb, instance_matching('IIMB'))).
:- db_add_novel(
  user:file_search_path(instance_matching, oaei2012('Instance matching'))
).

:- debug(iimb).



%! load_alignment_iimb is det.
% Loads all the IIMB alignments into memory.

load_alignment_iimb:-
  forall(
    between(1, 80, Integer),
    load_alignment_iimb(Integer, _SVG)
  ).

%! load_alignment_iimb(+Number:nonneg, -SVG) is det.
% Loads a specific IIMB alignment into memory and exports the IOTW results.

load_alignment_iimb(Integer, SVG):-
  load_shared_iimb(Integer, Graph, Alignments),
  debug(iotw, 'Loaded graph ~w.', [Graph]),
  alignments_by_predicates(Graph, Alignments, Predicates),
  debug(iotw, '  Alignments established for graph ~w.', [Graph]),
  export_rdf_alignments(Graph, Alignments, Predicates, SVG),
  debug(iotw, '  Exported alignments for graph ~w.', [Graph]).

%! load_shared_iimb(+Number:nonneg, -Graph:atom, -Alignments:list) is det.
% @param Number The number of the IIMB alignments that is loaded.
% @param Graph The name of the RDF graph into which the aligned data
%        is loaded.
% @param Alignments ...

load_shared_iimb(Integer, OntologyGraph, Alignments):-
  between(1, 80, Integer), !,

  % Create a unique graph name.
  format(atom(GraphSuggestion), 'iimb_~w', [Integer]),
  rdf_new_graph(GraphSuggestion, OntologyGraph),

  % Load the base ontology.
  absolute_file_name(iimb(onto), BaseFile, [access(read), file_type(owl)]),
  rdf_load2(BaseFile, [graph(OntologyGraph)]),
  %rdf_load2(BaseFile, [graph(iotw_temp_1)]),

  % Load the aligned ontology.
  format_integer(Integer, 3, SubDirName),
  absolute_file_name(
    iimb(SubDirName),
    SubDir,
    [access(read), file_type(directory)]
  ),
  absolute_file_name(
    onto,
    AlignedOntologyFile,
    [access(read), file_type(owl), relative_to(SubDir)]
  ),
  rdf_load2(AlignedOntologyFile, [graph(OntologyGraph)]),
  %rdf_load2(AlignedOntologyFile, [graph(iotw_temp_2)]),
  %rdf_graph_merge([iotw_temp_1, iotw_temp_2], Graph),
  %maplist(rdf_unload_graph, [iotw_temp_1, iotw_temp_2]),

  % Load the reference alignments between the base ontology and
  % the aligned ontology.
  absolute_file_name(
    refalign,
    AlignmentsFile,
    [access(read), file_type(rdf), relative_to(SubDir)]
  ),
  oaei_file_to_alignments(AlignmentsFile, Alignments).



