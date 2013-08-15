:- module(
  iotw_pairs_export,
  [
    export_rdf_shared/4 % +Graph:atom
                        % +Alignments:list(pair)
                        % +Stash
                        % -GIF:compound
  ]
).

/** <module> IOTW_PAIRS_EXPORT

Exports the results of classifying *all* resource pairs
by the predicates they share.

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(iotw(iotw_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).



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
    [color(black),label(NilNodeLabel),shape(rectangle),style(solid)],

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
