:- module(
  iotw_relatedness,
  [
    export_rdf_alignments/3, % +Graph:atom
                             % +Alignments:list(pair)
                             % +Assoc:assoc
    export_rdf_alignments/3, % +Out:oneof([atom,stream])
                             % +Graph:atom
                             % +Assoc:assoc
    rdf_alignment_share/3, % +Graph:atom
                           % +Alignments:list(pair)
                           % -Predicates:assoc
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
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(standards(graphviz)).

:- rdf_meta(same_object(r,r)).
:- rdf_meta(same_predicate(r,r)).



export_rdf_alignments(Graph, Alignments, Assoc):-
  absolute_file_name(data(Graph), File, [access(write), file_type(graphviz)]),
  export_rdf_alignments(File, Graph, Alignments, Assoc).

%! export_rdf_alignments(
%!   +Out:oneof([atom,stream]),
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   +Assoc:assoc
%! ) is det.

export_rdf_alignments(Stream, Graph, Alignments, Assoc):-
  is_stream(Stream),
  !,

  % Establish the predicate sets that are part of the hierarchy.
  assoc_to_keys(Assoc, SomeKeys),
  setoff(
    Key,
    (
      member(SomeKey, SomeKeys),
      sublist(Key, SomeKey)
    ),
    Keys
  ),

  % The number of pairs (for statistics).
  %rdf_subjects(Graph, Subjects),
  %cardinality(Subjects, NumberOfSubjects),
  %NumberOfPairs is NumberOfSubjects ** 2,

  % The number of identity pairs (for statistics).
  cardinality(Alignments, NumberOfIdentityPairs),

  % Separate the nil node from the non-nil nodes.
  NilKey = [],
  selectchk(NilKey, Keys, NonNilKeys),

  % Nodes: nil nodes.
  NilRank = rank(node(r0, NilRankNodeAttributes), [NilNode]),
  NilRankNodeAttributes = [label(0), shape(plaintext)],
  NilNode = node(NilNodeID, NilNodeAttributes),
  sha_hash_atom(NilKey, NilHash),
  format(atom(NilNodeID), 'n~w', [NilHash]),
  assoc:get_assoc(NilKey, Assoc, NilValues),
  cardinality(NilValues, NumberOfNilIdentityPairs),
  LinkedNilPercentage is NumberOfNilIdentityPairs / NumberOfIdentityPairs,
  format(
    atom(NilNodeLabel),
    '{} [~d/~d=~2f]',
    [NumberOfNilIdentityPairs, NumberOfIdentityPairs, LinkedNilPercentage]
  ),
  NilNodeAttributes =
    [color(black), label(NilNodeLabel), shape(rectangle), style(solid)],

  % Extract the ranks that occur in the hierarchy.
  setoff(
    RankNumber,
    (
      member(Key, NonNilKeys),
      cardinality(Key, RankNumber)
    ),
    RankNumbers
  ),
  % Nodes: non-nil nodes.
  findall(
    rank(node(RankNodeID, RankNodeAttributes), ContentNodes),
    (
      member(RankNumber, RankNumbers),
      format(atom(RankNodeID), 'r~w', [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankNodeAttributes = [label(RankLabel), shape(plaintext)],
      % Consider only keys of the rank length.
      length(Key, RankNumber),
      findall(
        node(NodeID, NodeAttributes),
        (
          member(Key, NonNilKeys),
          % Establish the node ID.
          sha_hash_atom(Key, Hash),
          format(atom(NodeID), 'n~w', [Hash]),
          % Include a description of the key.
          rdf_resource_naming(Key, KeyLabel),
          % Retrieve the key's associated values.
          unless(
            assoc:get_assoc(Key, Assoc, TheseIdentityPairs),
            TheseIdentityPairs = []
          ),
          % We like to include some numbers in the node label.
          cardinality(TheseIdentityPairs, NumberOfTheseIdentityPairs),
          % There may be many pairs that are not in the alignment.
          predicates_to_pairs(Graph, Key, ThesePairs),
          cardinality(ThesePairs, NumberOfThesePairs),
          Percentage1 is NumberOfTheseIdentityPairs / NumberOfIdentityPairs,
          Percentage2 is NumberOfTheseIdentityPairs / NumberOfThesePairs,
          format(
            atom(NodeLabel),
            '~w [~d/~d=~2f] [~d/~d=~2f]',
            [
              KeyLabel,
              NumberOfTheseIdentityPairs,
              NumberOfIdentityPairs,
              Percentage1,
              NumberOfTheseIdentityPairs,
              NumberOfThesePairs,
              Percentage2
            ]
          ),
          NodeAttributes =
            [color(blue), label(NodeLabel), shape(rectangle), style(solid)]
        ),
        ContentNodes
      )
    ),
    NonNilRanks
  ),

  % Edges
  EdgeAttributes = [color(black), style(solid)],
  findall(
    edge(FromNodeID, ToNodeID, EdgeAttributes),
    (
      member(FromKey, Keys),
      length(FromKey, FromLength),
      ToLength is FromLength + 1,
      length(ToKey, ToLength),
      member(ToKey, Keys),
      ord_subset(FromKey, ToKey),
      sha_hash_atom(FromKey, FromHash),
      format(atom(FromNodeID), 'n~w', [FromHash]),
      sha_hash_atom(ToKey, ToHash),
      format(atom(ToNodeID), 'n~w', [ToHash])
    ),
    Edges
  ),
gtrace,
  % Graph properties
  GraphAttributes =
    [
      charset('UTF-8'),
      fontsize(11.0),
      label(Graph),
      overlap(false)
    ],
  stream_graphviz(Stream, graph([], [NilRank | NonNilRanks], Edges, GraphAttributes)).
% If a file is given, then the export is first streamed to it and
% then converted to a displayable format.
export_rdf_alignments(File, Graph, Alignments, Assoc):-
  access_file(File, write),
  !,
  open(File, write, Stream),
  export_rdf_alignments(Stream, Graph, Alignments, Assoc),
  close(Stream),
  once(file_type_alternative(File, pdf, PDF_File)),
  convert_graphviz(File, dot, pdf, PDF_File).

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

%! linked_percentage(
%!   +NumberOfAllPairsAndAllPairs:pair(integer,list(pair)),
%!   +Alignments:list(pair),
%!   -Percentage:float
%! ) is det.
% Returns the percentage of given pairs that are also in
% the given alignments.

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

rdf_shared(Graph, Alignments):-
  rdf_shared_pairs(Graph, Tuples),
  export_rdf_shared(Graph, Alignments, Tuples).

%! rdf_shared(+Graph:atom, +Alignments:list(pair), -SVG:dom) is det.
% Returns the SVG representation of the shared properties between pairs
% in the given graph, plus an annotation of the alignment overlap.

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

%! rdf_alignment_share(
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   -Predicates:assoc
%! ) is det.

rdf_alignment_share(Graph, Alignments, Predicates):-
  empty_assoc(EmptyAssoc),
  rdf_alignment_share(Graph, EmptyAssoc, Alignments, Predicates).

%! rdf_alignment_share(
%!   +Graph:atom,
%!   +OldPredicates:assoc,
%!   +Alignments:list(pair),
%!   -NewPredicates:assoc
%! ) is det.

rdf_alignment_share(_Graph, SolAssoc, [], SolAssoc):-
  !.
rdf_alignment_share(Graph, OldAssoc, [From-To | Alignments], SolAssoc):-
  rdf_shared_pair(From, To, Predicates),
  put_assoc(Predicates, OldAssoc, From-To, NewAssoc),
  rdf_alignment_share(Graph, NewAssoc, Alignments, SolAssoc).

%! rdf_shared_pair(
%!   +Subject1:uri,
%!   +Subject2:uri,
%!   -Predicates:ordset(uri)
%! ) is det.

rdf_shared_pair(Subject1, Subject2, Solution):-
  rdf_shared_pair(Subject1, Subject2, [], Solution).

rdf_shared_pair(Subject1, Subject2, OldPredicates, Solution):-
  % We assume a fully materialized graph.
  rdf(Subject1, Predicate1, Object1, Graph),
  % We are looking for new predicates only.
  \+ member(Predicate1, OldPredicates),
  same_predicate(Predicate1, Predicate2),
  rdf(Subject2, Predicate2, Object2, Graph),
  Subject1 @< Subject2,
  same_object(Object1, Object2),
  !,
  ord_add_element(OldPredicates, Predicate1, NewPredicates),
  rdf_shared_pair(Subject1, Subject2, NewPredicates, Solution).
rdf_shared_pair(_Subject1, _Subject2, Predicates, Predicates).



%! pair_to_predicates(
%!   +Graph:atom,
%!   +Pair:pair,
%!   -Predicates:ordset(uri)
%! ) is det.

pair_to_predicates(G, Ps, X-Y):-
  pair_to_predicates(G, [], Ps, X-Y).

pair_to_predicates(G, Ps, Sol, X-Y):-
  rdf(X, P1, O1, G),
  % We are looking for new predicates only.
  \+ member(P1, Ps),
  same_predicate(P1, P2),
  rdf(Y, P2, O2, G),
  X @< Y,
  same_object(O1, O2),
  !,
  pair_to_predicates(G, [P1 | Ps], Sol, X-Y).
pair_to_predicates(_G, Ps, Ps, _X-_Y).

%! predicates_to_pair(
%!   +Graph:atom,
%!   +Predicates:ordset(uri),
%!   -Pair:pair
%! ) is nondet.

predicates_to_pair(G, [P11 | Ps], X-Y):-
  rdf(X, P11, O11, G),
  rdf(Y, P12, O12, G),
  same_predicate(P11, P12),
  X @< Y,
  same_object(O11, O12),
  forall(
    member(P21, Ps),
    (
      rdf(X, P21, O21, G),
      rdf(Y, P22, O22, G),
      same_predicate(P21, P22),
      X @< Y,
      same_object(O21, O22)
    )
  ).

%! predicates_to_pairs(
%!   +Graph:atom,
%!   +Predicates:ordset(uri),
%!   -Pairs:list(pair)
%! ) is det.

predicates_to_pairs(G, Ps, Pairs):-
  findall(
    Pair,
    predicates_to_pair(G, Ps, Pair),
    Pairs
  ).

same_object(O, O).

same_predicate(P, P).

