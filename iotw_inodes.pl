:- module(
  iotw_inodes,
  [
    assert_identity_nodes/3, % +Graph:atom
                             % +AlignmentPairs:list(pair)
                             % -GraphAlignmentPairsHash:atom
    calculate_quality/2, % +GraphAlignmentPairsHash:atom
                         % -Quality:between(0.0,1.0)
    graph_alignment/6, % ?GraphAlignmentPairsHash:atom
                       % ?RDF_Graph:atom
                       % ?AlignmentPairs:list(pair)
                       % ?Predicates:assoc
                       % ?NumberOfIdentityPairs:nonneg
                       % ?NumberOfPairs:nonneg
    identity_node/6, % ?GraphAlignmentsPairsKeyHash:atom
                     % ?GraphAlignmentPairsHash:atom
                     % ?Key:ordset(iri)
                     % ?InHigherApproximation:boolean
                     % ?NumberOfIdentityPairs:nonneg
                     % ?NumberOfPairs:nonneg
    possible_to_calculate_quality/1, % ?GraphAlignmentPairsHash:atom
    update_identity_node/1 % +GAK_Hash:atom
  ]
).

/** <module> IOTW_ALIGNMENTS

Classifies alignment resource pairs by the predicates they share.

## Stage 1

Input: The identity relation / a list of alginment pairs.

Output: An association list with predicate sets as keys and
        subsets of the alignment pairs as values.

## Extending the identity relation

Possible extensions of the alignment pairs:
  1. Non-identity pairs in the lower-minus-higher approximation.
  2. Non-identity pairs in proper supersets of the lower approximation.

@author Wouter Beek
@tbd Treat alignment sets, not alignment pairs (e.g. the properties that are
     shared by three resources.
@tbd Typed literals are the equivalent if the canonical mappings
     of their inverse lexical mappings are the same.
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_term)).

%! graph_alignment(
%!   ?GraphAlignmentPairsHash:atom,
%!   ?RDF_Graph:atom,
%!   ?AlignmentPairs:list(pair),
%!   ?Predicates:assoc,
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?NumberOfPairs:nonneg
%! ) is nondet.

:- dynamic(graph_alignment/6).

%! identity_node(
%!   ?GraphAlignmentsPairsKeyHash:atom,
%!   ?GraphAlignmentPairsHash:atom,
%!   ?Key:ordset(iri),
%!   ?InHigherApproximation:boolean,
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?NumberOfPairs:nonneg
%! ) is nondet.

:- dynamic(identity_node/6).



%! assert_identity_nodes(
%!   +Graph:atom,
%!   +AlignmentPairs:list(pair),
%!   -GraphAlignmentPairsHash:atom
%! ) is det.
% Returns an association list with predicate keys
% and alignment pair set values.

assert_identity_nodes(G, A, GA_Hash):-
  % We can identify this RDF graph and alignment pairs combination later
  % using a hash.
  variant_sha1(G-A, GA_Hash),

  % Calculate the number of identity pairs.
  length(A, NumberOfIdentityPairs),

  % Calculate the number of pairs.
  setoff(S, (rdf_subject(G, S), \+ rdf_is_bnode(S)), Ss),
  length(Ss, NumberOfS),
  NumberOfPairs is NumberOfS ** 2,

  % Calculate the predicate sets that are part of the hierarchy.
  alignment_sets_by_predicates(G, A, PsAssoc),
  assoc_to_keys(PsAssoc, Keys),
  maplist(assert_node(GA_Hash, G, PsAssoc), Keys),

  assert(
    graph_alignment(GA_Hash,G,A,PsAssoc,NumberOfIdentityPairs,NumberOfPairs)
  ).

%! alignment_sets_by_predicates(
%!   +Graph:atom,
%!   +AlignmentSets:list(ordset(iri)),
%!   -Predicates:assoc
%! ) is det.
% @see Wrapper around alignment_sets_by_predicates/4.

alignment_sets_by_predicates(G, A_Sets, SolAssoc):-
  empty_assoc(EmptyAssoc),
  alignment_sets_by_predicates(G, EmptyAssoc, A_Sets, SolAssoc).

%! alignment_sets_by_predicates(
%!   +Graph:atom,
%!   +OldPredicates:assoc,
%!   +AlignmentSets:list(ordset(iri)),
%!   -NewPredicates:assoc
%! ) is det.

alignment_sets_by_predicates(_G, SolAssoc, [], SolAssoc):- !.
alignment_sets_by_predicates(G, OldAssoc, [A_Set|A_Sets], SolAssoc):-
  % Take the predicates that the alignment pair shares.
  rdf_shared_properties(G, A_Set, SharedPreds),
  % Add the alignment pair as a value to the predicates key.
  put_assoc(SharedPreds, OldAssoc, A_Set, NewAssoc),
  alignment_sets_by_predicates(G, NewAssoc, A_Sets, SolAssoc).

%! assert_node(
%!   +GraphAlignmentHash:atom,
%!   +Graph:atom,
%!   +GroupedBySharedPreds:assoc,
%!   +SharedPreds:ordset
%! ) is det.

assert_node(GA_Hash, G, Assoc, SharedPreds):-
  variant_sha1(GA_Hash-Key, GAK_Hash),

  % Count the identity pairs and percentage.
  (
    assoc:get_assoc(SharedPreds, Assoc, IdSets)
  ;
    IdSets = []
  ), !,
  aggregate(
    sum(IdSetSize),
    (
      member(IdSet, IdSets),
      length(IdSet, IdSetSize)
    ),
    NumberOfIdenticals
  ),

  % Check whether this identity node belongs to the lower or to the
  % higher approximation.
  % It belongs to the lower approximation if there is
  % at least one member that shares the given properties but does not
  % belong to the identity relation.
  (
    rdf_shares_properties(G, SharedPreds, IdSets, _SimS1, _SimS2)
  ->
    InHigher = true,
    NumberOfEquivalents = NumberOfIdenticals
  ;
    InHigher = false
  ),

  % -- say it --
  assert(
    identity_node(
      GAK_Hash,
      GA_Hash,
      Key,
      InHigher,
      NumberOfIdenticals,
      NumberOfEquivalents
    )
  ).

%! predicates_to_set(
%!   +Graph:atom,
%!   +SharedPreds:ordset(iri),
%!   -SharePreds:ordset
%! ) is nondet.
% Returns a pair of resources that shares all and only the given predicates.
%
% Retrieve the number of pairs of resources that share those
% and only those predicates in the key.
% Notice that we are now looking at *all* pairs,
% not only those in the alignments.
%
% @tbd See whether this can be optimized using profile/1.

predicates_to_set(G, Ps, SolSet):-
  ord_empty(S),
  predicates_to_set(G, Ps, S, SolSet).
% @tbd
predicates_to_set(G, [P1|Ps], X, Y):-
  % Find two resources that share the first predicate.
  rdf(X, P1, O1, G),
  rdf(Y, P1, O1, G),
  % We only need pairs that are ordered in this way (no symmetric instances).
  X @< Y,
  % Now check whether they share the other predicates as well.
  forall(
    member(P2, Ps),
    (
      rdf(X, P2, O2, G),
      rdf(Y, P2, O2, G)
    )
  ).

%! predicates_to_sets(
%!   +Graph:atom,
%!   +Predicates:ordset(uri),
%!   -Sets:list(ordset(iri))
%! ) is det.
% Returns all pairs of resources that share all and only
% the given predicates.

predicates_to_sets(G, Ps, Sets):-
  findall(Set, predicates_to_set(G, Ps, Set), Sets).

%! rdf_shared_properties(
%!   +Graph:atom,
%!   +Set:ordset(iri),
%!   -Predicates:ordset(uri)
%! ) is det.
% Returns the predicates that both subjects possess.
%
% Moves from sets of resources to the shared properties of those resources.
%
% @see Wrapper around rdf_shared_properties/4.

rdf_shared_properties(G, Set, SolPs):-
  rdf_shared_properties(G, Set, [], SolPs).

rdf_shared_properties(G, [X|Set], OldPs, SolPs):-
  % We assume a fully materialized graph.
  rdf(X, P, O, G),
  % We are looking for new predicates only.
  \+ memberchk(P, OldPs),
  % All subject terms in the set must share these properties.
  % Succeeds if the given subject term has the given property
  % (expressed by a predicate and object term).
  forall(member(Y, Set), rdf(Y, P, O, G)), !,
  ord_add_element(OldPs, P, NewPs),
  rdf_shared_properties(G, [X|Set], NewPs, SolPs).
rdf_shared_properties(_G, _Set, SolPs, SolPs).

%! rdf_shares_properties(
%!   +Graph:atom,
%!   +SharedPreds:ordset(iri),
%!   +IdentitySet:ordset(iri),
%!   -SharedPreds1:iri,
%!   -SharedPreds2:iri
%! ) is det.
% Returns a single pair that shares the given predicates,
% but that does not belong to the given identity set.

rdf_shares_properties(G, [P1|Ps], IdSets, SimS1, SimS2):-
  % They share at least one property
  rdf(SimS1, P1, O, G),
  rdf(SimS2, P1, O, G),
  % They are not the same.
  % Discarding symmetric results.
  SimS1 @< SimS2,
  % They are not in any of the identity sets.
  \+ ((
    member(IdSet, IdSets),
    member(SimS1, SimS2, IdSet)
  )),
  % They share all properties.
  forall(member(P2, Ps), (rdf(SimS1, P2, O2, G), rdf(SimS2, P2, O2, G))).

update_identity_node(GAK_Hash):-
  once(
    identity_node(
      GAK_Hash,
      GA_Hash,
      Key,
      InHigher,
      NumberOfIdenticals,
      NumberOfEquivalents1
    )
  ),
  once(graph_alignment(GA_Hash,G,A_Sets,_,_,_)),
  var(NumberOfEquivalents1), !,
  predicates_to_sets(G, A_Sets, Sets),
  aggregate(
    sum(Length),
    (
      member(Set, Sets),
      length(Set, Length)
    ),
    NumberOfEquivalents2
  ),
  db_replace_novel(
    identity_node(
      GAK_Hash,
      GA_Hash,
      Key,
      InHigher,
      NumberOfIdenticals,
      NumberOfEquivalents1),
    identity_node(
      GAK_Hash,
      GA_Hash,
      Key,
      InHigher,
      NumberOfIdenticals,
      NumberOfEquivalents2
    )
  ).



% SUPPORT PREDICATES %

calculate(GA_Hash, InHigher, Cardinality):-
  aggregate(
    sum(N),
    identity_node(_,GA_Hash,_,InHigher,_,N),
    Cardinality
  ).

calculate_higher(GA_Hash, Cardinality):-
  calculate(GA_Hash, true, Cardinality).

calculate_lower(GA_Hash, Cardinality):-
  calculate(GA_Hash, _, Cardinality).

calculate_quality(GA_Hash, Quality):-
  calculate_higher(GA_Hash, HigherCardinality),
  calculate_lower(GA_Hash, LowerCardinality),
  % Make sure we never divide by zero.
  (  HigherCardinality =:= LowerCardinality
  -> Quality = 1.0
  ;  Quality = HigherCardinality / LowerCardinality).

possible_to_calculate(GA_Hash, InHigher):-
  forall(
    identity_node(_,GA_Hash,_,InHigher,_,NumberOfKeyPairs),
    nonvar(NumberOfKeyPairs)
  ).

possible_to_calculate_higher(GA_Hash):-
  possible_to_calculate(GA_Hash, true).

possible_to_calculate_lower(GA_Hash):-
  possible_to_calculate(GA_Hash, _).

possible_to_calculate_quality(GA_Hash):-
  possible_to_calculate_higher(GA_Hash),
  possible_to_calculate_lower(GA_Hash).

