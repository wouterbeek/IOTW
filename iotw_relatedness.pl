:- module(
  iotw_relatedness,
  [
    rdf_shared/1, % +Graph:atom
    rdf_shared/2, % +Graph:atom
                  % +Alignments:list(pair)
    rdf_shared/3 % +Graph:atom
                 % +Alignments:list(pair)
                 % -SVG:dom
  ]
).

/** <module>

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(standards(graphviz)).



%! export_rdf_shared(
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   +Stash:list(tuple)
%! ) is det.

export_rdf_shared(Graph, Alignments, Stash):-
  absolute_file_name(data(Graph), File, [access(write), file_type(graphviz)]),
  export_rdf_shared(File, Graph, Alignments, Stash).

%! export_rdf_shared(
%!   +In:oneof([file,stream]),
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   +Stash:list(tuple)
%! ) is det.

export_rdf_shared(Stream, Graph, Alignments, Stash):-
  is_stream(Stream),
  !,

  rdf_subjects(Graph, Subjects),
  cardinality(Subjects, NumberOfSubjects),
  NumberOfAllPairs is NumberOfSubjects ** 2,

  % Nodes: Nil node
  % @tbd Establish the number of nil pairs (and the percentage).
  NilRank = rank(node(r0, NilRankNodeAttributes), [NilNode]),
  NilRankNodeAttributes = [label(0), shape(plaintext)],
  NilNode = node(n0, NilNodeAttributes),
  format(
    atom(NilNodeLabel),
    '{} (100%) (~d/~d)',
    [NumberOfAllPairs, NumberOfAllPairs]
  ),
  NilNodeAttributes =
    [color(black), label(NilNodeLabel), shape(rectangle), style(solid)],

  % Nodes: Non-nil nodes
  % We put these in ranks, based on the cardinality of the predicate set.
  setoff(
    RankNumber,
    member(_-RankNumber-_-_, Stash),
    RankNumbers
  ),
  findall(
    rank(node(RankNodeID, RankNodeAttributes), ContentNodes),
    (
      member(RankNumber, RankNumbers),
      format(atom(RankNodeID), 'r~w', [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankNodeAttributes = [label(RankLabel), shape(plaintext)],
      findall(
        node(NodeID, NodeAttributes),
        (
          nth1(I, Stash, PSet-RankNumber-NumberOfThesePairs-ThesePairs),
          linked_percentage(
            NumberOfThesePairs-ThesePairs,
            Alignments,
            LinkedPercentage
          ),
          rdf_resource_naming(PSet, PSetLabel),
          % Retrieve the ordered set, not on of its members!
          Percentage is NumberOfThesePairs / NumberOfAllPairs * 100,
          format(atom(NodeID), 'n~w', [I]),
          format(
            atom(NodeLabel),
            '~w (~2f%) (~d/~d) [~2f]',
            [PSetLabel, Percentage, NumberOfThesePairs, NumberOfAllPairs, LinkedPercentage]
          ),
          NodeAttributes =
            [color(blue), label(NodeLabel), shape(rectangle), style(solid)],
          parse_attributes_graphviz(node, NodeAttributes)
        ),
        ContentNodes
      )
    ),
    Ranks
  ),

  % Edges: Nil-edges
  EdgeAttributes = [color(black), style(solid)],
  parse_attributes_graphviz(edge, EdgeAttributes),
  findall(
    edge(n0, ToNode, EdgeAttributes),
    (
      member(PSet-1-NumberOfThesePairs-ThesePairs, Stash),
      once(nth1(I, Stash, PSet-1-NumberOfThesePairs-ThesePairs)),
      format(atom(ToNode), 'n~w', [I])
    ),
    NilEdges
  ),

  % Edges: Non-nil edges
  findall(
    edge(FromNode, ToNode, EdgeAttributes),
    (
      member(PSet1-NumberOfPs1-NumberOfPairs1-Pairs1, Stash),
      succ(NumberOfPs1, NumberOfPs2),
      member(PSet2-NumberOfPs2-NumberOfPairs2-Pairs2, Stash),
      ord_subset(PSet1, PSet2),
      once(nth1(I, Stash, PSet1-NumberOfPs1-NumberOfPairs1-Pairs1)),
      format(atom(FromNode), 'n~w', [I]),
      once(nth1(J, Stash, PSet2-NumberOfPs2-NumberOfPairs2-Pairs2)),
      format(atom(ToNode), 'n~w', [J])
    ),
    NonNilEdges
  ),

  % Graph properties
  GraphAttributes =
    [
      charset('UTF-8'),
      fontsize(11.0),
      label(Graph),
      overlap(false)
    ],
  parse_attributes_graphviz(graph, GraphAttributes),

  append(NilEdges, NonNilEdges, Edges),
  stream_graphviz(
    Stream,
    graph([], [NilRank | Ranks], Edges, GraphAttributes)
  ).
% If a file is given, then the export is first streamed to it and
% then converted to a displayable format.
export_rdf_shared(File, Graph, Alignments, In):-
  access_file(File, write),
  !,
  open(File, write, Stream),
  export_rdf_shared(Stream, Graph, Alignments, In),
  close(Stream),
  once(file_type_alternative(File, pdf, PDF_File)),
  convert_graphviz(File, dot, pdf, PDF_File).

linked_percentage(NumberOfAllPairs-AllPairs, Alignments, Percentage):-
  setoff(
    X-Y,
    (
      member(X-Y, Alignments),
      once((
        member(X-Y, AllPairs)
      ;
        member(Y-X, AllPairs)
      ))
    ),
    LinkedPairs
  ),
  cardinality(LinkedPairs, NumberOfLinkedPairs),
  Percentage is NumberOfLinkedPairs / NumberOfAllPairs.

rdf_shared(Graph):-
  rdf_shared(Graph, []).

rdf_shared(Graph, Alignments):-
  rdf_shared_pairs(Graph, Tuples),
  export_rdf_shared(Graph, Alignments, Tuples).

rdf_shared(Graph, Alignments, SVG):-
  rdf_shared_pairs(Graph, Tuples),
  absolute_file_name(
    personal(temp),
    File,
    [access(write), file_type(graphviz)]
  ),
  open(File, write, Stream, []),
  export_rdf_shared(File, Graph, Alignments, Tuples),
  graphviz_to_svg(File, dot, SVG),
  close(Stream),
  delete_file(File).

%! rdf_shared_pairs(+Graph:atom, -Tuples:list) is det.
% Loads the pairs that share the same properties.
%
% # Example
%
% Given the following 4 triples:
% ==
% <S1, P, O12>
% <S2, P, O12>
% <S3, P, O34>
% <S4, P, O34>
% ==
%
% The triples would be:
% ==
% <{P},{<S1,S2>,<S3,S4>}>
% ==

%! rdf_shared(+Graph:atom, -Stash:tuple) is det.
% @arg Stash A tuple of the following form:
%      =|Predicates-NumberOfPredicates-NumberOfPairs-Pairs|=

rdf_shared_pairs(Graph, Stash):-
  rdf_predicates(Graph, Predicates),
  findall(
    [Predicate]-1-Size-SingletonPairs,
    (
      member(Predicate, Predicates),
      findall(
        Subject1-Subject2,
        (
          % We assume a fully materialized graph.
          rdf(Subject1, Predicate, Object, Graph),
          rdf(Subject2, Predicate, Object, Graph),
          Subject1 @< Subject2
        ),
        SingletonPairs
      ),
      cardinality(SingletonPairs, Size),
      % Sometimes predicates have no extension. Exclude these early on.
      Size > 0
    ),
    SingletonTuples
  ),
  rdf_shared_pairs(SingletonTuples, SingletonTuples, [], Stash).

rdf_shared_pairs([], SingletonTuples, TempStash, SolStash):-
  !,
  append(SingletonTuples, TempStash, SolStash).
rdf_shared_pairs(
  [PSet-NumberOfPs-_NumberOfPairs-Pairs | Tuples],
  SingletonTuples,
  TempStash,
  SolStash
):-
  findall(
    NewEntry,
    (
      % Find a single predicate that we can add to an existing set of predicates.
      member(SingletonPSet-1-_-SingletonPairs, SingletonTuples),
      \+ ord_subset(SingletonPSet, PSet),
      ord_union(PSet, SingletonPSet, NewPSet),
      % Since a bigger set can be obtained as a result of the union of
      % multiple smaller sets, we make sure that we do not include any
      % duplicates.
      \+ member(NewPSet-_-_-_, TempStash),

      % If the intersection is not empty, then we have found a new stash.
      ord_intersection(Pairs, SingletonPairs, NewPairs),
      \+ ord_empty(NewPairs),

      % The extended set of predicates may be further extensible.
      NewNumberOfPs is NumberOfPs + 1,
      cardinality(NewPairs, NewNumberOfPairs),
      NewEntry = NewPSet-NewNumberOfPs-NewNumberOfPairs-NewPairs
    ),
    NewEntries
  ),

  append(Tuples, NewEntries, NewTuples),
  append(TempStash, NewEntries, NewTempStash),
  rdf_shared_pairs(NewTuples, SingletonTuples, NewTempStash, SolStash).

