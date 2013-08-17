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
@version 2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(iotw(iotw)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).

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
  cardinality(A, NumberOfIdentityPairs),

  % Calculate the number of pairs.
  setoff(S, (rdf_subject(G, S), \+ rdf_is_bnode(S)), Ss),
  cardinality(Ss, NumberOfS),
  NumberOfPairs is NumberOfS ** 2,
  
  % Calculate the predicate sets that are part of the hierarchy.
  alignment_pairs_by_predicates(G, A, PsAssoc),
  assoc_to_keys(PsAssoc, Keys),
  maplist(assert_node(GA_Hash, G, PsAssoc), Keys),
  
  assert(graph_alignment(GA_Hash,G,A,PsAssoc,NumberOfIdentityPairs,NumberOfPairs)).

%! alignment_pairs_by_predicates(
%!   +Graph:atom,
%!   +AlignmentPairs:list(pair),
%!   -Predicates:assoc
%! ) is det.

alignment_pairs_by_predicates(G, A, SolAssoc):-
  empty_assoc(EmptyAssoc),
  alignment_pairs_by_predicates(G, EmptyAssoc, A, SolAssoc).

%! alignment_pairs_by_predicates(
%!   +Graph:atom,
%!   +OldPredicates:assoc,
%!   +AlignmentPairs:list(pair),
%!   -NewPredicates:assoc
%! ) is det.

alignment_pairs_by_predicates(_G, SolAssoc, [], SolAssoc):- !.
alignment_pairs_by_predicates(G, OldAssoc, [From-To|A], SolAssoc):-
  % Take the predicates that the alignment pair shares.
  rdf_shared_pair(G, From, To, Ps),
  % Add the alignment pair as a value to the predicates key.
  put_assoc(Ps, OldAssoc, From-To, NewAssoc),
  alignment_pairs_by_predicates(G, NewAssoc, A, SolAssoc).

assert_node(GA_Hash, G, PsAssoc, Key):-
  variant_sha1(GA_Hash-Key, GAK_Hash),
  
  % Count the identity pairs and percentage.
  (
    assoc:get_assoc(Key, PsAssoc, KeyIdentityPairs), !
  ;
    KeyIdentityPairs = []
  ),
  cardinality(KeyIdentityPairs, NumberOfKeyIdentityPairs),
  
  % Whether the identity node belongs to the lower approximation or not.
  % The identity node belongs to the lower approximation if there is
  % at least one pair that shares the given predicates but does not
  % belong to the identity relation.
  (
    predicates_to_pair(G, Key, From-To),
    \+ member(From-To, KeyIdentityPairs)
  ->
    InHigher = false
  ;
    InHigher = true,
    NumberOfKeyPairs = NumberOfKeyIdentityPairs
  ),
  
  % -- say it --
  assert(identity_node(GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs)).

%! predicates_to_pair(
%!   +Graph:atom,
%!   +Predicates:ordset(uri),
%!   -Pair:pair
%! ) is nondet.
% Returns a pair of resources that shares all and only the given predicates.
%
% Retrieve the number of pairs of resources that share those
% and only those predicates in the key.
% Notice that we are now looking at *all* pairs,
% not only those in the alignments.
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

%! rdf_shared_pair(
%!   +Graph:atom,
%!   +Subject1:uri,
%!   +Subject2:uri,
%!   -Predicates:ordset(uri)
%! ) is det.
% Returns the predicates that both subjects possess.
%
% Moves from pairs to predicates.

rdf_shared_pair(G, S1, S2, SolPs):-
  rdf_shared_pair(G, S1, S2, [], SolPs).

rdf_shared_pair(G, S1, S2, OldPs, SolPs):-
  % We assume a fully materialized graph.
  rdf(S1, P1, O1, G),
  % We are looking for new predicates only.
  \+ member(P1, OldPs),
  same_predicate(P1, P2),
  rdf(S2, P2, O2, G),
  % @tbd If the alignments are assured to be ordered in a specific way,
  %      then `@<` could be used here instead (more efficient).
  S1 \== S2,
  same_object(O1, O2),
  !,
  ord_add_element(OldPs, P1, NewPs),
  rdf_shared_pair(G, S1, S2, NewPs, SolPs).
rdf_shared_pair(_G, _S1, _S2, Ps, Ps).

update_identity_node(GAK_Hash):-
  once(identity_node(GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs1)),
  once(graph_alignment(GA_Hash,G,A,_,_,_)),
  var(NumberOfKeyPairs1), !,
  predicates_to_pairs(G, A, Pairs),
  cardinality(Pairs, NumberOfKeyPairs2),
  db_replace_novel(
    identity_node(GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs1),
    identity_node(GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs2)
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
  calculate(GA_Hash, false, Cardinality).

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
  possible_to_calculate(GA_Hash, false).

possible_to_calculate_quality(GA_Hash):-
  possible_to_calculate_higher(GA_Hash),
  possible_to_calculate_lower(GA_Hash).

