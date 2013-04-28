module(
  iotw,
  [
    check_anatomy/0,
    load_anatomy/0
  ]
).

/** <module> IDENTITY ON THE WEB

My first publication with Stephan and Frank!

@author Wouter Beek
@version 2013/04
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_entailment)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_test)).
:- use_module(server(wallace)).
:- use_module(standards(oaei)).

:- rdf_register_prefix(oboRel, 'http://www.obofoundry.org/ro/ro.owl#').
:- rdf_register_prefix(oboInOwl, 'http://www.geneontology.org/formats/oboInOwl#').



check_anatomy:-
  oaei_graph_to_alignments(reference, ReferenceAlignments),

  % Write the header of the table to user output.
  format(user_output, '\tOverlap\tFalsePositives\tFalseNegatives\n', []),
  flush_output(user_output),

  forall(
    oaei_graph(AlignmentGraph),
    (
      format(user_output, '[~w]\n', [AlignmentGraph]),

      % The given alignment relative to the reference alignment.
      oaei_graph_to_alignments(AlignmentGraph, RawAlignments),
      oaei_check_alignment(ReferenceAlignments, RawAlignments),
      
      % The upper alignment relative to the reference alignment.
      upper(from, to, AlignmentGraph, UpperAlignments),
      oaei_check_alignment(ReferenceAlignments, UpperAlignments)
    )
  ).

load_anatomy:-
  % From
  absolute_file_name(
    anatomy(mouse),
    FromFile,
    [access(read), file_type(owl), register_namespaces(true)]
  ),
  rdf_load(FromFile, [graph(from)]),

  % To
  absolute_file_name(
    anatomy(human),
    ToFile,
    [access(read), file_type(owl), register_namespaces(true)]
  ),
  rdf_load(ToFile, [graph(to)]),

  % Reference
  absolute_file_name(
    anatomy(reference),
    ReferenceFile,
    [access(read), file_type(rdf)]
  ),
  rdf_load2(ReferenceFile),

  % Raw alignments
  absolute_file_name(
    anatomy_raw(.),
    RawDirectory,
    [access(read), file_type(directory)]
  ),
  rdf_load2(RawDirectory),

  % Many data files have invalid namespace notation!
  rdf_expand_namespace(_, _, _, _).

save_data:- %DEB
  % Make sure we have all namespaces asserted when we save triples to file.
  rdf_register_namespaces,
  absolute_file_name(
    auto(.),
    Directory,
    [access(read), file_type(directory)]
  ),
  rdf_save2(Directory).



/*
% PROPERTY PATHS %

compare_property_paths(X, OnlyX, Y, OnlyY, Shared):-
  setoff(
    PropertyPathX,
    property_path(X, PropertyPathX),
    PropertyPathsX
  ),
  setoff(
    PropertyPathY,
    property_path(Y, PropertyPathY),
    PropertyPathsY
  ),
  ord_intersect(PropertyPathsX, PropertyPathsY, Shared),
  ord_subtract(PropertyPathsX, Shared, OnlyX),
  ord_subtract(PropertyPathsY, Shared, OnlyY).

% Leibniz' Law.
% The principle of the indiscernability of identicals.
leibniz_law(X, Y):-
  owl_resource_identity(X, Y),
  forall(
    property_path(X, PropertyPath),
    property_path(Y, PropertyPath)
  ),
  forall(
    property_path(Y, PropertyPath),
    property_path(X, PropertyPath)
  ).

property_path(Subject, [Property | Properties]):-
  rdf(Subject, Property, Object),
  rdf_is_bnode(Object),
  property_path(Object, Properties).
property_path(Subject, [Property-Object]):-
  rdf(Subject, Property, Object),
  rdf_is_literal(Object).
property_path(Subject, [Property-Object]):-
  rdf(Subject, Property, Object),
  rdf_is_resource(Object).

shared_property_paths(X, Y, Shared):-
  compare_property_paths(X, _OnlyX, Y, _OnlyY, Shared).
*/



% UPPER %

upper(FromGraph, ToGraph, AlignmentGraph, UpperAlignments):-
  % Make sure this is an alignment graph.
  oaei_graph(AlignmentGraph),
  oaei_graph_to_alignments(AlignmentGraph, Alignments),
  empty_assoc(EmptyAssoc),
  put_alignments(Alignments, EmptyAssoc, Assoc),
  assoc_to_keys(Assoc, Propertiess),
  findall(
    UpperAlignment,
    (
      member(Properties, Propertiess),
      shared_properties(FromGraph, ToGraph, Properties, UpperAlignment)
    ),
    UpperAlignments
  ).

%% put_alignments(+Alignments:list(list), +OldAssoc, -NewAssoc) is det.

put_alignments([], Assoc, Assoc):-
  !.
put_alignments([[From, To] | Alignments], OldAssoc, FinalAssoc):-
  shared_properties([From, To], Shared),
  put_assoc(Shared, OldAssoc, From-To, NewAssoc),
  put_alignments(Alignments, NewAssoc, FinalAssoc).



% SHARED PROPERTIES %

%% shared_properties(+Pair:list, -Properties:list(list)) is det.

shared_properties([X, Y], Properties):-
  var(Properties),
  !,
  setoff(
    Property-Value1,
    (
      rdf(X, Property, Value1),
      %%%%% Exclude properties that occur multiple times for the same subject.
      %%%%\+ (rdf(X, Property, Value2), Value2 \== Value1),
      rdf(Y, Property, Value1)
    ),
    Properties
  ).

%% shared_properties(
%%   +FromGraph:atom,
%%   +ToGraph:atom,
%%   +Properties:ord_set(list),
%%   -Pair:list
%% ) is nondet.

shared_properties(
  FromGraph,
  ToGraph,
  [FirstProperty-FirstValue | Properties],
  [X, Y]
):-
  nonvar(Properties),
  !,
  % Find an X that has all the properties.
  rdf(X, FirstProperty, FirstValue, FromGraph),
  forall(
    member(Property-Value, Properties),
    rdf(X, Property, Value)
  ),
  % Find another Y that has all the properties as well.
  rdf(Y, FirstProperty, FirstValue, ToGraph),
  X \== Y,
  forall(
    member(Property-Value, Properties),
    rdf(Y, Property, Value)
  ).



/* TMP
  setoff(
    [FromResourceUpper, ToResourceUpper],
    (
      % Take a pair that is not in the alignment graph.
      rdf_subject(FromGraph, FromResourceUpper),
      rdf_subject(ToGraph, ToResourceUpper),
      \+ member([FromResourceUpper, ToResourceUpper], Alignments),
      shared_properties(FromResourceUpper, ToResourceUpper, SharedProperties),
      % There is a pair in the alignment graph with the same properties.
      member([FromResource, ToResource], Alignments),
      shared_properties(FromResource, ToResource, SharedProperties)
    ),
    UpperAlignment
  ).

test1:-
  rdf_global_id(dbpedia:'Izaak_H._Reijnders', X),
  assert_resource(X, iotw),
  rdf_global_id(dbpedia:'Didier_Reynders', Y),
  assert_resource(Y, iotw),

  format(user, 'SHARED PROPERTIES\n', []),
  setoff(P, shared_property(X, Y, P), Ps),
  print_list(user, Ps),

  format(user, 'SHARED PROPERTY VALUES\n', []),
  setoff(PV, shared_property_value(X, Y, PV), PVs),
  print_list(user, PVs).

test2:-
  formulate_sparql(
    [],
    'SELECT DISTINCT ?s',
    ['  ?s a foaf:Person .'],
    0,
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Rows),
  run_on_sublists(Rows, identity:assert_persons).

assert_persons(List):-
  maplist(assert_person, List).

assert_person(row(Person)):-
  thread_self(SelfId),
  assert_resource(Person, SelfId),
  thread_success(SelfId).
*/

