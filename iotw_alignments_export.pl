:- module(
  iotw_alignments_export,
  [
    export_rdf_alignments/4, % +Graph:atom
                             % +Alignments:list(pair)
                             % +Assoc:assoc
                             % -SVG:dom
    predicates_to_pairs/3 % +Graph:atom
                          % +Predicates:ordset(uri)
                          % -Pairs:list(pair)
  ]
).

/** <module> IOTW_ALIGNMENTS_EXPORT

Exports the results of classifying alignment resource pairs
by the predicates they share.

@author Wouter Beek
@tbd Add rank to the left-hand side of the graphic order using GraphViz edges.
@version 2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(gv(gv_file)).
:- use_module(gv(gv_hash)).
:- use_module(iotw(iotw_export)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(sha)).
:- use_module(rdf(rdf_graph)).
:- use_module(xml(xml_dom)).



%! export_rdf_alignments(
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   +Assoc:assoc,
%!   -SVG:dom
%! ) is det.
% Returns the SVG DOM representation of the hierarchy of predicate (sub)sets
% annotated with the number of resource pairs that share those and only those
% predicates.
%
% @tbd Add callback function injection.
%      Add to function: RDF_Graph, AssocHash.

export_rdf_alignments(RDF_Graph, Alignments, Assoc, SVG2):-
  rdf_graph(RDF_Graph), !,
  export_rdf_alignments_(RDF_Graph, Alignments, Assoc, GIF),
  
  % Create an arbitrary but quite unique name for the association list
  % and register it under that name.
  term_to_atom(Assoc, Atom),
  sha_hash(Atom, HashCodes, []),
  hash_atom(HashCodes, AssocHash),
  register_assoc(AssocHash, Assoc),
  
  graph_to_svg_dom([], GIF, dot, SVG1),
  xml_inject_dom_with_attribute(SVG1, node, [onclick='function()'], SVG2).

%! export_rdf_alignments_(
%!   +RDF_Graph:atom,
%!   +Alignments:list(pair),
%!   +Assoc:assoc,
%!   -GIF:compound
%! ) is det.

export_rdf_alignments_(RDF_Graph, Alignments, Assoc, GIF):-
  % Clear the data of any previous export.
  init_export,

  % Reset the indexed SHA hash map.
  clear_indexed_sha_hash,

  % Establish the predicate sets that are part of the hierarchy.
  assoc_to_keys(Assoc, Keys),

  % The number of identity pairs (for statistics).
  cardinality(Alignments, NumberOfIdentityPairs),

  % Ranks: the nul rank.
  NilRankNodeID = r0,
  NilRank =
    rank(vertex(NilRankNodeID,NilRankNodeID,NilRankNodeAttributes),[NilNode]),
  NilRankNodeAttributes = [label('0'),shape(plaintext)],

  % Calculate the number of pairs.
  setoff(
    Subject,
    (
      rdf_subject(RDF_Graph, Subject),
      \+ rdf_is_bnode(Subject)
    ),
    Subjects
  ),
  cardinality(Subjects, NumberOfSubjects),
  NumberOfPairs is NumberOfSubjects ** 2,

  % Calculate the number of nil pairs.
  NumberOfNonIdentityPairs is NumberOfPairs - NumberOfIdentityPairs,

  % Separate the nil key from the non-nil keys.
  NilKey = [],
  (selectchk(NilKey, Keys, NonNilKeys), ! ; NonNilKeys = Keys),

  % Build nodes: build the nil node.
  build_vertex(
    Assoc,
    NilKey,
    NumberOfIdentityPairs,
    NumberOfNonIdentityPairs,
    NilNode
  ),

  % Extract the ranks that occur in the hierarchy.
  setoff(
    RankNumber,
    (
      member(Key, NonNilKeys),
      cardinality(Key, RankNumber)
    ),
    RankNumbers
  ),

  % Build nodes: build the non-nil nodes.
  findall(
    rank(vertex(RankNodeID,RankNodeID,RankNodeAttributes),ContentNodes),
    (
      % The rank.
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

          % Retrieve the number of pairs of resources that share those
          % and only those predicates in the key.e
          % Notice that we are now looking at *all* pairs,
          % not only those in the alignments.
          predicates_to_pairs(RDF_Graph, Key, ThesePairs),
          cardinality(ThesePairs, NumberOfThesePairs),

          build_vertex(
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
  EdgeAttributes = [color(black),style(solid)],
  findall(
    edge(FromNodeID, ToNodeID, EdgeAttributes),
    (
      member(ToKey, Keys),
      strict_sublist(FromKey, ToKey),
      member(FromKey, [NilKey|Keys]),
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

  % Calculate the accuracy.
  findall(
    NumberOfPairsInLower1,
    node(_, _, _, NumberOfPairsInLower1, true),
    NumberOfPairsInLower2
  ),
  sum_list(NumberOfPairsInLower2, NumberOfPairsInLower3),
  findall(
    NumberOfPairsInHigher1,
    node(_, _, _, NumberOfPairsInHigher1, false),
    NumberOfPairsInHigher2
  ),
  sum_list(NumberOfPairsInHigher2, NumberOfPairsInHigher3),
  (
    NumberOfPairsInHigher3 =:= 0
  ->
    Accuracy = 0
  ;
    Accuracy is NumberOfPairsInLower3 / NumberOfPairsInHigher3
  ),

  % Graph properties.
  format(atom(GraphLabel), 'Name: ~w   Accuracy: ~e', [RDF_Graph,Accuracy]),
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
      [graph_name(RDF_Graph) | GraphAttributes]
    ).

%! predicates_to_pair(
%!   +Graph:atom,
%!   +Predicates:ordset(uri),
%!   -Pair:pair
%! ) is nondet.
% Returns a pair of resources that shares all and only the given predicates.
%
% @tbd See whether this can be optimized using profile/1.

predicates_to_pair(G, [P11|Ps], X-Y):-
  % Find two resources that share the first predicate.
  rdf(X, P11, O11, G),
  same_predicate(P11, P12),
  rdf(Y, P12, O12, G),
  same_object(O11, O12),
  % We only need pairs that are ordered in this way (no symmetric instances).
  X @< Y,
  % Now check whether they share the other predicates as well.
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
% Returns all pairs of resources that share all and only
% the given predicates.

predicates_to_pairs(G, Ps, Pairs):-
  findall(
    Pair,
    predicates_to_pair(G, Ps, Pair),
    Pairs
  ).

