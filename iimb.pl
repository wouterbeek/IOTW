:- module(
  iimb,
  [
    iimb/3 % +Options:list(nvpair)
           % +Number:nonneg
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
:- use_module(iotw(iotw)).
:- use_module(library(apply)).
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



%! iimb(+Options:list(nvpair), +Number:between(1,80), -SVG:list) is det.
% Loads a specific IIMB alignment into memory and exports the IOTW results.
%
% @param Options A list of name-value pairs.
% @param Number The number of the IIMB dataset that is used.
% @param SVG A list of compound terms describing an SVG DOM.

iimb(O, N, SVG):-
  % Retrieve an RDF graph and a set of alignment pairs.
  load_shared_iimb(N, G, A_Sets),
  run_experiment(O, G, A_Sets, SVG, _PDF_File).

%! load_shared_iimb(
%!   +Number:nonneg,
%!   -Graph:atom,
%!   -AlignmentSets:list(ordset(iri))
%! ) is det.
% @param Number The number of the IIMB alignments that is loaded.
% @param Graph The name of the RDF graph into which the alignments are loaded.
% @param AlignmentSets A list of ordered sets of aligned resources.

load_shared_iimb(N, OntologyGraph, A_Sets):-
  between(1, 80, N), !,

  % Clear the temporary RDF graphs we use for graph merge.
  maplist(rdf_unload_graph, [iotw_temp_1,iotw_temp_2]),

  % Create a unique graph name.
  format(atom(GraphSuggestion), 'iimb_~w', [N]),
  rdf_new_graph(GraphSuggestion, OntologyGraph),

  % Load the base ontology.
  absolute_file_name(iimb(onto), BaseFile, [access(read), file_type(owl)]),
  rdf_load2(BaseFile, [graph(iotw_temp_1)]),

  % Load the aligned ontology.
  format_integer(N, 3, SubDirName),
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
    A_File,
    [access(read),file_type(rdf),relative_to(SubDir)]
  ),
  oaei_file_to_alignments(A_File, _A_Pairs, A_Sets).

