module(
  iotw,
  [
    load_shared_iimb/2 % +Integer:integer
                       % -SVG:dom
  ]
).

/** <module> IDENTITY ON THE WEB

My first publication with Stephan and Frank!

# Identity & equality

## Non-equal identicals

In the value space of XSD floats, NaN is identical with itself, but
not equivalent with itself.

## Non-identical equals

In the value space of XSD floats, -0 and 0 are equal but not identical.

@author Wouter Beek
@version 2013/04-2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(project(iotw_relatedness)).
:- use_module(rdf(rdf_serial)).
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

:- http_handler(root(node), node, []).

:- debug(iotw).



load_alignment_anatomy:-
  maplist(rdf_unload_graph, [human, mouse]),
  flag(anatomy, ID, ID + 1),
  format(atom(Graph), 'anatomy_~w', [ID]),
  maplist(
    xml_register_namespace,
    [
      rdfs,
      oboRel,
      owl,
      xsd,
      rdf,
      oboInOwl
    ],
    [
      'http://www.w3.org/2000/01/rdf-schema#',
      'http://www.obofoundry.org/ro/ro.owl#',
      'http://www.w3.org/2002/07/owl#',
      'http://www.w3.org/2001/XMLSchema#',
      'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      'http://www.geneontology.org/formats/oboInOwl#'
    ]
  ),
  db_add_novel(user:file_search_path(anatomy, oaei2012('Anatomy'))),
  db_add_novel(user:file_search_path(raw_results, anatomy('Raw results'))),
  absolute_file_name(
    anatomy(human),
    HumanFile,
    [access(read), file_type(owl)]
  ),
  rdf_load2(HumanFile, [graph(human)]),
  absolute_file_name(
    anatomy(mouse),
    MouseFile,
    [access(read), file_type(owl)]
  ),
  rdf_load2(MouseFile, [graph(mouse)]),
  rdf_graph_merge([human, mouse], Graph),
  absolute_file_name(
    raw_results(.),
    AlignmentDir,
    [access(read), file_type(directory)]
  ),
  path_walk_tree(AlignmentDir, '.*.rdf$', AlignmentFiles),
  forall(
    member(AlignmentFile, AlignmentFiles),
    (
      oaei_file_to_alignments(AlignmentFile, Alignments),
      rdf_alignment_share(Graph, Alignments, Predicates),
      export_rdf_alignments(Graph, Alignments, Predicates, _SVG)
    )
  ).

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

%! node(+Request:list(nvpair)) is det.
% Callback HTTP handler reaction on a click action.

node(Request):-
  member(search(SearchParameters), Request),
  if_then(
    member(id=Id, SearchParameters),
    node0(Id)
  ).

node0(Id1):-
  sub_atom(Id1, 6, _Length, 0, Id2),
  indexed_sha_hash(Key, Id2),
  iotw_relatedness:current_assoc(Assoc),
  assoc:get_assoc(Key, Assoc, TheseIdentityPairs),
  iotw_relatedness:current_graph(Graph),
  iotw_relatedness:predicates_to_pairs(Graph, Key, ThesePairs),
  ord_subtract(ThesePairs, TheseIdentityPairs, TheseNonIdentityPairs),
  findall(
    DOM,
    (
      member(TheseNonIdentityPair, TheseNonIdentityPairs),
      iotw_relatedness:pair_to_dom(TheseNonIdentityPair, DOM)
    ),
    DOMs
  ),
  append(DOMs, DOM),
  push(console_output, DOM).

