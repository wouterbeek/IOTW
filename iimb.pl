:- module(
  iimb,
  [
    iimb/2 % +Number:nonneg
           % -SVG:dom
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(gv(gv_file)).
:- use_module(iotw(iotw_inodes)).
:- use_module(iotw(iotw_export)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
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



%! iimb is det.
% Loads all the IIMB alignments into memory.

iimb:-
  forall(
    between(1, 80, Integer),
    iimb(Integer, _SVG)
  ).

%! iimb(+Number:nonneg, -SVG) is det.
% Loads a specific IIMB alignment into memory and exports the IOTW results.

iimb(N, SVG):-
  % Retrieve an RDF graph and a set of alignment pairs.
  load_shared_iimb(N, G, A),
  debug(iimb, 'Loaded graph ~w.', [G]),

  % Returns the RDF graph and alignment pairs hash.
  assert_identity_nodes(G, A, GA_Hash),
  debug(iimb, '  Alignments established for graph ~w.', [G]),

  % Create an SVG representation for the given hash.
  export_identity_nodes(GA_Hash, SVG),
  debug(iimb, '  Exported alignments for graph ~w.', [G]).

%! load_shared_iimb(
%!   +Number:nonneg,
%!   -Graph:atom,
%!   -AlignmentSets:list(ordset(iri))
%! ) is det.
% @param Number The number of the IIMB alignments that is loaded.
% @param Graph The name of the RDF graph into which the alignments are loaded.
% @param AlignmentSets A list of ordered sets of aligned resources.

load_shared_iimb(Integer, OntologyGraph, AlignmentSets):-
  between(1, 80, Integer), !,

  % Clear the temporary RDF graphs we use for graph merge.
  maplist(rdf_unload_graph, [iotw_temp_1,iotw_temp_2]),

  % Create a unique graph name.
  format(atom(GraphSuggestion), 'iimb_~w', [Integer]),
  rdf_new_graph(GraphSuggestion, OntologyGraph),

  % Load the base ontology.
  absolute_file_name(iimb(onto), BaseFile, [access(read), file_type(owl)]),
  rdf_load2(BaseFile, [graph(iotw_temp_1)]),

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
  rdf_load2(AlignedOntologyFile, [graph(iotw_temp_2)]),

  % Merge the two ontology graphs.
  % @tbd Why is the module needed here?!
  rdf_graph:rdf_graph_merge([iotw_temp_1,iotw_temp_2], OntologyGraph),
  maplist(rdf_unload_graph, [iotw_temp_1, iotw_temp_2]),

  % Load the reference alignments between the base ontology and
  % the aligned ontology.
  absolute_file_name(
    refalign,
    AlignmentsFile,
    [access(read), file_type(rdf), relative_to(SubDir)]
  ),
  oaei_file_to_alignment_sets(AlignmentsFile, AlignmentSets).

