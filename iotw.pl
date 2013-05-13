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
@version 2013/04-2013/05
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_clean)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_statistics)).
:- use_module(rdf(rdf_tms)).
:- use_module(server(wallace)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(oboRel, 'http://www.obofoundry.org/ro/ro.owl#').
:- xml_register_namespace(oboInOwl,
                          'http://www.geneontology.org/formats/oboInOwl#').



% Assumption 1: Reference alignment loaded in graph =reference=.
% Assumption 2: A linked dataset is loaded in graph =from=.
% Assumption 3: Another linked dataset is loaded in graph =to=.
check:-
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
  db_add_novel(user:file_search_path(anatomy, oaei2012('Anatomy'))),

  % From
  absolute_file_name(
    anatomy(mouse),
    FromFile,
    [access(read), file_type(owl)]
  ),
  rdf_load2(FromFile, [format(xml), graph(from), register_namespaces(true)]),

  % To
  absolute_file_name(anatomy(human), ToFile, [access(read), file_type(owl)]),
  rdf_load2(ToFile, [format(xml), graph(to), register_namespaces(true)]),

  % Reference
  absolute_file_name(
    anatomy(reference),
    ReferenceFile,
    [access(read), file_type(rdf)]
  ),
  rdf_load2(ReferenceFile, [graph(reference), register_namespaces(true)]),

  % Raw alignments
  absolute_file_name(
    anatomy('Raw results'),
    RawDirectory,
    [access(read), file_type(directory)]
  ),
  rdf_load2(RawDirectory),

  % Many data files have invalid namespace notation! Method from RDF_CLEAN.
  rdf_expand_namespace(_, _, _, _),

  % Bring statistics into view.
  check.

load_instance_matching:-
  rdf_register_prefix(
    'IIMBTBOX',
    'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
  ),
  db_add_novel(
    user:file_search_path(instance_matching, oaei2012('Instance matching'))
  ),
  db_add_novel(user:file_search_path(iimb, instance_matching('IIMB'))),

  % From
  absolute_file_name(iimb(onto), FromFile, [access(read), file_type(owl)]),
  rdf_load(FromFile, [graph(from), register_namespaces(true)]),

  % An ontology is linked against another ontology from one of the
  % subdirectories.
  forall(
    (
      between(1, 80, Index),
      format_integer(Index, 3, SubdirectoryName)
    ),
    (
      maplist(rdf_unload_graph, [reference,to]),
      X =.. [iimb, SubdirectoryName],
      absolute_file_name(X, Subdirectory, [file_type(directory)]),
      % To
      absolute_file_name(
        onto,
        ToFile,
        [access(read), file_type(owl), relative_to(Subdirectory)]
      ),
      rdf_load(ToFile, [graph(to), register_namespaces(true)]),
      % Reference
      absolute_file_name(
        refalign,
        ReferenceFile,
        [access(read), file_type(rdf), relative_to(Subdirectory)]
      ),
      rdf_load2(ReferenceFile, [graph(reference), register_namespaces(true)]),
      % Bring statistics into view.
      check
    )
  ).



% SHARED PROPERTIES %

shared_properties(Graph):-
  AssocName = shared_properties,
  register_assoc(AssocName),
  forall(
    (
      rdf_subject(Graph, Subject1),
      rdf_subject(Graph, Subject2),
      Subject1 \== Subject2
    ),
    (
      setoff(
        Property,
        (
          rdf(Subject1, Property, Object, Graph),
          rdf(Subject2, Property, Object, Graph)
        ),
        Properties
      ),
      put_assoc(Properties, AssocName, [Subject1, Subject2])
    )
  ).

%! shared_properties(+Pair:list, -Properties:list(list)) is det.

shared_properties([X, Y], Properties):-
  var(Properties),
  !,
  setoff(
    Property-Value,
    (
      rdf(X, Property, Value),
      %%%%! Exclude properties that occur multiple times for the same subject.
      %%%%\+ (rdf(X, Property, OtherValue), OtherValue \== Value),
      rdf(Y, Property, Value)
    ),
    Properties
  ).

%! shared_properties(
%!   +FromGraph:atom,
%!   +ToGraph:atom,
%!   +ReferenceGraph:atom, %DEB
%!   +Properties:ord_set(list),
%!   -Pair:list
%! ) is nondet.

shared_properties(
  FromGraph,
  ToGraph,
  ReferenceGraph, %DEB
  PredicateObjectPairs,
  [X, Y]
):-
  nonvar(PredicateObjectPairs),
  !,
  first(PredicateObjectPairs, FirstPredicate-FirstObject),
  % Find an X that has all the properties.
  rdf(X, FirstPredicate, FirstObject, FromGraph),
  forall(
    member(Predicate-Object, PredicateObjectPairs),
    rdf(X, Predicate, Object)
  ),
  % Find another Y that has all the properties as well.
  rdf(Y, FirstPredicate, FirstObject, ToGraph),
  X \== Y,
  forall(
    member(Predicate-Object, PredicateObjectPairs),
    rdf(Y, Predicate, Object)
  ),
  %DEB
  (
    \+ oaei_alignment(ReferenceGraph, X, Y)
  ->
    format(user_output, 'NEW PAIR:\n\t~w\n\t~w\n', [X, Y]),
    maplist(
      print_property(FromGraph, ToGraph),
     [FirstPredicate-FirstObject | PredicateObjectPairs]
    ),
    count_subjects(PredicateObjectPairs, [FromGraph, ToGraph], These),
    count_subjects(_AnyPredicate, _AnyObject, [FromGraph, ToGraph], Any),
    format(user_output, '[~w/~w] COMBINED\n\n', [These, Any])
  ;
    true
  ).

print_property(Graph1, Graph2, Predicate-Object):-
  abbreviate(Predicate, PredicateAbbr),
  abbreviate(Object, ObjectAbbr),
  count_subjects(Predicate, Object, [Graph1, Graph2], These),
  count_subjects(_AnyPredicate1, _AnyObject1, [Graph1, Graph2], Any),
  format(
    user_output,
    '[~w/~w] ~w---~w\n',
    [These, Any, PredicateAbbr, ObjectAbbr]
  ).

abbreviate(literal(lang(Lang, Value)), Abbr):-
  !,
  format(atom(Abbr), '~w^^~w', [Value, Lang]).
abbreviate(literal(type(Type, Value)), Abbr):-
  !,
  abbreviate(Type, TypeAbbr),
  format(atom(Abbr), '~w^^~w', [Value, TypeAbbr]).
abbreviate(literal(Abbr), Abbr):-
  !.
abbreviate(URI, Abbr):-
  rdf_global_id(Namespace:Local, URI),
  !,
  format(atom(Abbr), '~w:~w', [Namespace, Local]).
abbreviate(Abbr, Abbr).



% UPPER %

upper(FromGraph, ToGraph, AlignmentGraph, UpperAlignments):-
  % Make sure this is an alignment graph.
  oaei_graph(AlignmentGraph),
  oaei_graph_to_alignments(AlignmentGraph, Alignments),
  empty_assoc(EmptyAssoc),
  put_alignments(Alignments, EmptyAssoc, Assoc),
  assoc_to_keys(Assoc, Propertiess),
%gtrace, %DEB
%print_list(user_output, Propertiess),
  findall(
    UpperAlignment,
    (
      member(Properties, Propertiess),
      shared_properties(
        FromGraph,
        ToGraph,
        AlignmentGraph,
        Properties,
        UpperAlignment
      )
    ),
    UpperAlignments
  ).

%! put_alignments(+Alignments:list(list), +OldAssoc, -NewAssoc) is det.

put_alignments([], Assoc, Assoc):-
  !.
put_alignments([[From, To] | Alignments], OldAssoc, FinalAssoc):-
  shared_properties([From, To], Shared),
  put_assoc(Shared, OldAssoc, From-To, NewAssoc),
  put_alignments(Alignments, NewAssoc, FinalAssoc).

