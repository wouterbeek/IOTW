module(
  iotw,
  [
    load_shared_iimb/2 % +Integer:integer
                       % -SVG:dom
  ]
).

/** <module> IDENTITY ON THE WEB

My first publication with Stephan and Frank!

@author Wouter Beek
@version 2013/04-2013/05
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(project(iotw_relatedness)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_statistics)).
:- use_module(rdf(rdf_tms)).
:- use_module(server(wallace)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:prolog_file_type(assoc, assoc)).

:- xml_register_namespace(oboRel, 'http://www.obofoundry.org/ro/ro.owl#').
:- xml_register_namespace(oboInOwl,
                          'http://www.geneontology.org/formats/oboInOwl#').

% Set the global stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(global, limit(2*10**9)).
% Set the local stack to 2GB. This requires a 64-bit machine and OS.
:- set_prolog_stack(local, limit(2*10**9)).

% Root
http:location(root, '/prasem/', []).

:- debug(iotw).



load_shared_iimb(Integer, SVG):-
  load_shared_iimb(Integer, Graph, Alignments),
  rdf_shared(Graph, Alignments, SVG).

load_shared_iimb(Integer, Graph, Alignments):-
  between(1, 80, Integer),
  format(atom(Graph), 'iimb_~w', [Integer]),
  % There may be some triples asserted in the graph already.
  rdf_unload_graph(Graph),
  xml_register_namespace(
    'IIMBTBOX',
    'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
  ),
  db_add_novel(
    user:file_search_path(instance_matching, oaei2012('Instance matching'))
  ),
  db_add_novel(user:file_search_path(iimb, instance_matching('IIMB'))),
  absolute_file_name(iimb(onto), BaseFile, [access(read), file_type(owl)]),
  rdf_load2(BaseFile, [graph(iotw_temp_1)]),
  format_integer(Integer, 3, SubDirName),
  absolute_file_name(
    iimb(SubDirName),
    SubDir,
    [access(read), file_type(directory)]
  ),
  absolute_file_name(
    onto,
    OWL_File,
    [access(read), file_type(owl), relative_to(SubDir)]
  ),
  absolute_file_name(
    refalign,
    RDF_File,
    [access(read), file_type(rdf), relative_to(SubDir)]
  ),
  rdf_load2(OWL_File, [graph(iotw_temp_2)]),
  rdf_graph_merge([iotw_temp_1, iotw_temp_2], Graph),
  maplist(rdf_unload_graph, [iotw_temp_1, iotw_temp_2]),
  oaei_file_to_alignments(RDF_File, Alignments).

load_shared_properties1:-
  Graph = test1,
  \+ rdf_graph(Graph),
  rdf_assert(rdf:a, rdf:p, rdf:z1, Graph),
  rdf_assert(rdf:b, rdf:p, rdf:z1, Graph),
  rdf_assert(rdf:c, rdf:p, rdf:z2, Graph),
  rdf_assert(rdf:d, rdf:p, rdf:z2, Graph),
  rdf_assert(rdf:e, rdf:p, rdf:f, Graph),
  rdf_shared(Graph),
  rdf_unload_graph(Graph).

load_shared_properties2:-
  Graph = test2,
  \+ rdf_graph(Graph),
  absolute_file_name(data('VoID'), File, [access(read), file_type(turtle)]),
  rdf_load2(File, [graph(Graph)]),
  rdf_shared(Graph),
  rdf_unload_graph(Graph).

load_shared_properties3:-
  absolute_file_name(data(.), DataDir, [access(read), file_type(directory)]),
  path_walk_tree(DataDir, '.*.owl$', DataFiles),
  forall(member(DataFile, DataFiles), rdf_load2(DataFile, [])),
  forall(rdf_graph(Graph), rdf_shared(Graph)).

load_alignment_iimb:-
  forall(
    between(1, 80, Integer),
    load_alignment_iimb(Integer, _SVG)
  ).

load_alignment_iimb(Integer, SVG):-
  load_shared_iimb(Integer, Graph, Alignments),
  debug(iotw, 'Loaded graph ~w.', [Graph]),
  rdf_alignment_share(Graph, Alignments, Predicates),
  debug(iotw, '  Alignments established for graph ~w.', [Graph]),
  export_rdf_alignments(Graph, Alignments, Predicates, SVG),
  debug(iotw, '  Exported alignments for graph ~w.', [Graph]).

