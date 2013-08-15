:- module(
  iotw_relatedness,
  [
% ALIGNMENTS %
    export_rdf_alignments/4, % +Graph:atom
                             % +Alignments:list(pair)
                             % +Assoc:assoc
                             % -SVG:dom
    pair_to_dom/2, % +Pair:pair(uri)
                   % -DOM:list
    rdf_alignment_share/3, % +Graph:atom
                           % +Alignments:list(pair)
                           % -Predicates:assoc
% SHARED %
    rdf_shared/1, % +Graph:atom
    rdf_shared/2, % +Graph:atom
                  % +Alignments:list(pair)
    rdf_shared/3 % +Graph:atom
                 % +Alignments:list(pair)
                 % -SVG:dom
  ]
).

/** <module> IOTW_RELATEDNESS

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(gv(gv_file)).
:- use_module(gv(gv_hash)).
:- use_module(html(html)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(run_ext)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).
:- use_module(xml(xml_dom)).

:- dynamic(current_assoc(_Assoc)).
:- dynamic(current_graph(_Graph)).
:- dynamic(current_lower(_Lower)).
:- dynamic(current_higher(_Higher)).

:- rdf_meta(same_object(r,r)).
:- rdf_meta(same_predicate(r,r)).



% ALIGNMENTS ONLY %

%! build_node(
%!   +Assoc:assoc,
%!   +Key:ordset(uri),
%!   +NumberOfIdentityPairs:integer,
%!   +NumberOfThesePairs:integer,
%!   -Node:element
%! ) is det.

build_node(
  Assoc,
  Key,
  NumberOfIdentityPairs,
  NumberOfThesePairs,
  node(NodeID, NodeAttributes)
):-
  % Create the key label that described the key.
  rdf_term_name(Key, KeyLabel),

  % Establish the node ID.
  indexed_sha_hash(Key, Hash),
  format(atom(NodeID), 'n~w', [Hash]),

  % Count the identity pairs and percentage.
  unless(
    assoc:get_assoc(Key, Assoc, TheseIdentityPairs),
    TheseIdentityPairs = []
  ),
  cardinality(TheseIdentityPairs, NumberOfTheseIdentityPairs),
  Percentage1 is NumberOfTheseIdentityPairs / NumberOfIdentityPairs,

  % Calculate the percentage of identity pairs relative to all pairs
  % in the partition set.
  Percentage2 is NumberOfTheseIdentityPairs / NumberOfThesePairs,

  % Add to higher approximation.
  assert(current_higher(NumberOfThesePairs)),

  % Add to lower approximation.
  if_then(
    NumberOfTheseIdentityPairs = NumberOfThesePairs,
    assert(current_lower(NumberOfTheseIdentityPairs))
  ),

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
    [color(blue), label(NodeLabel), shape(rectangle), style(solid)].

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

rdf_alignment_share(_Graph, SolAssoc, [], SolAssoc):- !.
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
  Subject1 \== Subject2,
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

%! export_rdf_alignments(
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   +Assoc:assoc,
%!   -SVG:dom
%! ) is det.
% @tbd Callback function injection.

export_rdf_alignments(RDF_Graph, Alignments, Assoc, SVG2):-
  rdf_graph(RDF_Graph), !,
  export_rdf_alignments_(RDF_Graph, Alignments, Assoc, GIF),
  graph_to_svg_dom([], GIF, dot, SVG1),
  db_replace_novel(current_assoc(Assoc)),
  db_replace_novel(current_graph(RDF_Graph)),
  xml_inject_dom_with_attribute(SVG1, node, [onclick='function()'], SVG2).

%! export_rdf_alignments_(
%!   +RDF_Graph:atom,
%!   +Alignments:list(pair),
%!   +Assoc:assoc,
%!   -GIF:compound
%! ) is det.

export_rdf_alignments_(RDF_Graph, Alignments, Assoc, GIF):-
  % Reset the indexed SHA hash map.
  clear_indexed_sha_hash,

  % Establish the predicate sets that are part of the hierarchy.
  assoc_to_keys(Assoc, Keys),

  % The number of identity pairs (for statistics).
  cardinality(Alignments, NumberOfIdentityPairs),

  % Ranks: the nul rank.
  NilRank = rank(node(r0, NilRankNodeAttributes), [NilNode]),
  NilRankNodeAttributes = [label(0), shape(plaintext)],

  % Calculate the number of pairs.
  setoff(Subject, rdf_subject(RDF_Graph, Subject), Subjects),
  cardinality(Subjects, NumberOfSubjects),
  NumberOfPairs is NumberOfSubjects ** 2,

  % Calculate the number of non-nil pairs.
  findall(
    SomeLength,
    (
      member(SomeKey, Keys),
      assoc:get_assoc(SomeKey, Assoc, SomeValues),
      cardinality(SomeValues, SomeLength)
    ),
    AllLengths
  ),
  sum_list(AllLengths, NumberOfNonNilPairs),

  % Calculate the number of nil pairs.
  NumberOfNilPairs is NumberOfPairs - NumberOfNonNilPairs,

  % Nodes: the nil node.
  build_node(
    Assoc,
    NilKey,
    NumberOfIdentityPairs,
    NumberOfNilPairs,
    NilNode
  ),

  % Separate the nil key from the non-nil keys.
  NilKey = [],
  (selectchk(NilKey, Keys, NonNilKeys), ! ; NonNilKeys = Keys),

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
        Node,
        (
          member(Key, NonNilKeys),

          % There may be many pairs that are not in the alignment.
          predicates_to_pairs(Graph, Key, ThesePairs),
          cardinality(ThesePairs, NumberOfThesePairs),

          build_node(
            Assoc,
            Key,
            NumberOfIdentityPairs,
            NumberOfThesePairs,
            Node
          )
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
      member(ToKey, Keys),
      strict_sublist(FromKey, ToKey),
      member(FromKey, [ NilKey | Keys]),
      \+ ((
        strict_sublist(MiddleKey, ToKey),
        member(MiddleKey, Keys),
        strict_sublist(FromKey, MiddleKey)
      )),
      indexed_sha_hash(FromKey, FromHash),
      format(atom(FromNodeID), 'n~w', [FromHash]),
      indexed_sha_hash(ToKey, ToHash),
      format(atom(ToNodeID), 'n~w', [ToHash])
    ),
    Edges
  ),

  % Calculate the accuracy of the identity relation.
  findall(Lower, current_lower(Lower), Lowers),
  sum_list(Lowers, Lower),
  findall(Higher, current_higher(Higher), Highers),
  sum_list(Highers, Higher),
  Accuracy is Lower / Higher,
  % Clean up.
  retractall(current_lower(_Lower)),
  retractall(current_higher(_Higher)),

  % Graph properties
  format(atom(GraphLabel), 'Name: ~w   Accuracy: ~e', [Graph, Accuracy]),
  GraphAttributes =
    [
      charset('UTF-8'),
      fontsize(11.0),
      label(GraphLabel),
      overlap(false)
    ],

  % The graph compound term.
  GIF =
    graph(
      [], % Unranked nodes.
      [NilRank | NonNilRanks],
      Edges,
      [graph_name(Graph) | GraphAttributes]
    ).

%! pair_to_dom(+Pair:pair(uri), -Markup:list) is det.

pair_to_dom(X-Y, Markup):-
  rdf_pairs(X, X_Pairs1),
  rdf_pairs(Y, Y_Pairs1),

  % The table of shared properties.
  select_shared_properties(
    X_Pairs1,
    Y_Pairs1,
    SharedPropertyPairs,
    X_Pairs2,
    Y_Pairs2
  ),
  list_to_table(
    [caption('Table showing the shared properties.'), header(true)],
    [['Predicate', 'Object'] | SharedPropertyPairs],
    SharedPropertyTable
  ),

  % The table of shared predicates.
  select_shared_predicates(
    X_Pairs2,
    Y_Pairs2,
    SharedPredicateTriples,
    X_Pairs3,
    Y_Pairs3
  ),
  list_to_table(
    [caption('Table showing the shared predicates.'), header(true)],
    [['Predicate', 'X-Object', 'Y-Object'] | SharedPredicateTriples],
    SharedPredicateTable
  ),

  % The table of exclusive X-properties.
  list_to_table(
    [caption('Table showing the exclusive X predicates.'), header(true)],
    [['X-Predicate', 'X-Object'] | X_Pairs3],
    X_Table
  ),

  % The table of exclusive Y-properties.
  list_to_table(
    [caption('Table showing the exclusive Y predicates.'), header(true)],
    [['Y-Predicate', 'Y-Object'] | Y_Pairs3],
    Y_Table
  ),

  Markup =
    [
      element(h1, [], ['X: ', X]),
      element(h1, [], ['Y: ', Y]),
      SharedPropertyTable,
      SharedPredicateTable,
      X_Table,
      Y_Table
    ].



% ALL SHARED PREDICATES, ANNOTATED WITH ALIGNMENTS %

export_rdf_shared(Graph, Alignments, Stash, GIF):-
  setoff(Subject, rdf_subject(Graph, Subject), Subjects),
  cardinality(Subjects, NumberOfSubjects),
  NumberOfPairs is NumberOfSubjects ** 2,

  % Nodes: Nil node
  % @tbd Establish the number of nil pairs (and the percentage).
  NilRank = rank(node(r0, NilRankNodeAttributes), [NilNode]),
  NilRankNodeAttributes = [label(0), shape(plaintext)],
  NilNode = node(n0, NilNodeAttributes),
  format(
    atom(NilNodeLabel),
    '{} (100%) (~d/~d)',
    [NumberOfPairs, NumberOfPairs]
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
          rdf_term_name(PSet, PSetLabel),
          % Retrieve the ordered set, not on of its members!
          Percentage is NumberOfThesePairs / NumberOfPairs * 100,
          format(atom(NodeID), 'n~w', [I]),
          format(
            atom(NodeLabel),
            '~w (~2f%) (~d/~d) [~2f]',
            [
              PSetLabel,
              Percentage,
              NumberOfThesePairs,
              NumberOfPairs,
              LinkedPercentage
            ]
          ),
          NodeAttributes =
            [color(blue),label(NodeLabel),shape(rectangle),style(solid)]
        ),
        ContentNodes
      )
    ),
    Ranks
  ),

  % Edges: Nil-edges
  EdgeAttributes = [color(black),style(solid)],
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

  % The graph compound term.
  append(NilEdges, NonNilEdges, Edges),
  GIF = graph([], [NilRank|Ranks], Edges, GraphAttributes).

%! linked_percentage(
%!   +NumberOfAllPairsAndAllPairs:pair(integer,list(pair)),
%!   +Alignments:list(pair),
%!   -Percentage:float
%! ) is det.
% Returns the percentage of given pairs that are also in
% the given alignments.

linked_percentage(NumberOfPairs-AllPairs, Alignments, Percentage):-
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
  Percentage is NumberOfLinkedPairs / NumberOfPairs.

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

rdf_shared(RDF_Graph, Alignments):-
  rdf_shared_pairs(RDF_Graph, Tuples),
  export_rdf_shared(RDF_Graph, Alignments, Tuples, GIF),
  graph_to_gv_file([], GIF, dot, pdf, PDF_File),
  open_pdf(PDF_File). %DEB

%! rdf_shared(+Graph:atom, +Alignments:list(pair), -SVG:dom) is det.
% Returns the SVG representation of the shared properties between pairs
% in the given graph, plus an annotation of the alignment overlap.

rdf_shared(RDF_Graph, Alignments, SVG):-
  rdf_shared_pairs(RDF_Graph, Tuples),
  export_rdf_shared(RDF_Graph, Alignments, Tuples, GIF),
  graph_to_svg_dom([], GIF, dot, SVG).

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

rdf_shared_pairs([], SingletonTuples, TempStash, SolStash):- !,
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

/*
% Test predicates for rdf_shared//1.

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
  forall(
    member(DataFile, DataFiles),
    rdf_load2(DataFile, [])
  ),
  forall(
    rdf_graph(Graph),
    rdf_shared(Graph)
  ).
*/

