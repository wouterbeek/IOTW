:- module(
  iotw_iimb,
  [
    iotw_iimb/3 % +Options:list(nvpair)
                % +Number:nonneg
                % -SVG:dom
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09, 2013/11
*/

:- use_module(ap(ap)).
:- use_module(ap(ap_act)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(iotw(iotw)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('IIMBTBOX', 'http://oaei.ontologymatching.org/2012/IIMBTBOX/').

:- db_add_novel(user:prolog_file_type('tar.gz', archive)).



%! iotw_iimb(+Options:list(nvpair), +Number:between(1,80), -SVG:list) is det.
% Loads a specific IIMB alignment into memory and exports the IOTW results.
%
% @param Options A list of name-value pairs.
% @param Number The number of the IIMB dataset that is used.
% @param SVG A list of compound terms describing an SVG DOM.

iotw_iimb(O1, N, SVG):-
  % Typecheck. There are 80 cases.
  between(1, 80, N),

  % Clear the temporary RDF graphs we use for graph merge.
  maplist(rdf_unload_graph, [iotw_temp_1,iotw_temp_2]),

  % Create a unique graph name.
  format(atom(GraphSuggestion), 'iimb_~w', [N]),
  rdf_new_graph(GraphSuggestion, OntologyGraph),

  ap(
    [process(iimb_cp),project(iotw)],
    [ap_stage([from(input,'OAEI2012',archive)], ap_extract_archive)]
  ),

  db_add_novel(user:file_search_path(oaei2012, iotw('OAEI2012'))),
  db_add_novel(user:file_search_path(instance_matching, oaei2012('Instance matching'))),
  db_add_novel(user:file_search_path(iimb, instance_matching('IIMB'))),

  % Load the base ontology.
  absolute_file_name(iimb(onto), BaseFile, [access(read),file_type(owl)]),
  rdf_load2(BaseFile, [graph(iotw_temp_1)]),

  % Load the aligned ontology.
  format_integer(N, 3, SubDirName),
  absolute_file_name(
    iimb(SubDirName),
    SubDir,
    [access(read),file_type(directory)]
  ),
  absolute_file_name(
    onto,
    AlignedOntologyFile,
    [access(read),file_type(owl),relative_to(SubDir)]
  ),
  rdf_load2(AlignedOntologyFile, [graph(iotw_temp_2)]),

  % Merge the two ontology graphs.
  rdf_graph_merge([iotw_temp_1,iotw_temp_2], OntologyGraph),
  maplist(rdf_unload_graph, [iotw_temp_1, iotw_temp_2]),

  % Load the reference alignments between the base ontology and
  % the aligned ontology.
  absolute_file_name(
    refalign,
    A_File,
    [access(read),file_type(rdf),relative_to(SubDir)]
  ),
  oaei_file_to_alignments(A_File, A_Pairs),

  % Now that all files are properly loaded,
  % we can run the experiment.
  run_experiment(O1, OntologyGraph, A_Pairs, SVG, _PDF_File).

