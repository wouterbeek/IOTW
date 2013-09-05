:- module(
  iotw_inodes,
  [
    assert_identity_nodes/3, % +Graph:atom
                             % +AlignmentPairs:list(pair)
                             % -GraphAlignmentPairsHash:atom
% DATA STORE ACCESS
    graph_alignment/7, % ?GraphAlignmentPairsHash:atom
                       % ?RDF_Graph:atom
                       % ?AlignmentPairs:list(pair)
                       % ?SharedPredicates:assoc
                       % ?SharedProperties:assoc
                       % ?NumberOfIdentityPairs:nonneg
                       % ?NumberOfPairs:nonneg
    identity_node/6, % ?GraphAlignmentsPairsKeyHash:atom
                     % ?GraphAlignmentPairsHash:atom
                     % ?Key:ordset(iri)
                     % ?InHigherApproximation:boolean
                     % ?NumberOfIdentityPairs:nonneg
                     % ?NumberOfPairs:nonneg
% SUPPORT PREDICATES
    calculate_quality/2, % +GraphAlignmentPairsHash:atom
                         % -Quality:between(0.0,1.0)
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
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_term)).

%! graph_alignment(
%!   ?GraphAlignmentPairsHash:atom,
%!   ?RDF_Graph:atom,
%!   ?AlignmentPairs:list(pair),
%!   ?SharedPredicates:assoc,
%!   ?SharedProperties:assoc,
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?NumberOfPairs:nonneg
%! ) is nondet.

:- dynamic(graph_alignment/7).

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
%!   +AlignmentSets:list(ordset(iri)),
%!   -GraphAlignmentPairsHash:atom
%! ) is det.
% Returns an association list with predicate keys
% and alignment pair set values.

assert_identity_nodes(G, A_Sets, GA_Hash):-
  clear_db,

  % We can identify this RDF graph and alignment pairs combination later
  % using a hash.
  variant_sha1(G-A_Sets, GA_Hash),

  % Calculate the number of identity pairs.
  length(A_Sets, NumberOfIdentityPairs),

  % Calculate the number of pairs.
  setoff(S, (rdf_subject(G, S), \+ rdf_is_bnode(S)), Ss),
  length(Ss, NumberOfS),
  NumberOfPairs is NumberOfS ** 2,

  % Calculate the predicate sets that are part of the hierarchy.
  alignment_sets_to_property_assocs(G, Mode, A_Sets, P_Assoc, PO_Assoc),
  assoc_to_keys(P_Assoc, Keys),
  maplist(assert_node(GA_Hash, G, P_Assoc, PO_Assoc), Keys),

  assert(
    graph_alignment(
      GA_Hash,
      G,
      A_Sets,
      P_Assoc,
      PO_Assoc,
      NumberOfIdentityPairs,
      NumberOfPairs
    )
  ).

%! alignment_sets_to_property_assocs(
%!   +Graph:atom,
%!   +Mode:oneof([p,po]),
%!   +AlignmentSets:list(ordset(iri)),
%!   -SharedPredicates:assoc,
%!   -SharedProperties:assoc
%! ) is det.
% @see Wrapper around alignment_sets_to_property_assocs/7.

alignment_sets_to_property_assocs(G, Mode, A_Sets, SharedPs, SharedPOs):-
  empty_assoc(EmptyP_Assoc),
  empty_assoc(EmptyPO_Assoc),
  alignment_sets_to_property_assocs(
    G,
    Mode,
    A_Sets,
    EmptyP_Assoc,
    SharedPs,
    EmptyPO_Assoc,
    SharedPOs
  ).

%! alignment_sets_to_property_assocs(
%!   +Graph:atom,
%!   +Mode:oneof([p,po]),
%!   +AlignmentSets:list(ordset(iri)),
%!   +OldSharedPredicates:assoc,
%!   -NewSharedPredicates:assoc,
%!   +OldSharedProperties:assoc,
%!   -NewSharedProperties:assoc
%! ) is det.

alignment_sets_to_property_assocs(
  _G,
  _Mode,
  [],
  SolP_Assoc,
  SolP_Assoc,
  SolPO_Assoc,
  SolPO_Assoc
):-
  !.
alignment_sets_to_property_assocs(
  G,
  Mode,
  [A_Set|A_Sets],
  OldP_Assoc,
  SolP_Assoc,
  OldPO_Assoc,
  SolPO_Assoc
):-
  % Take the predicates that the alignment pair shares.
  rdf_shared(G, Mode, A_Set, SharedPs, SharedPOs),
  % Add the alignment pair as a value to the predicates key.
  put_assoc(SharedPs,  OldP_Assoc,  A_Set, NewP_Assoc ),
  put_assoc(SharedPOs, OldPO_Assoc, A_Set, NewPO_Assoc),
  alignment_sets_to_property_assocs(
    G,
    Mode,
    A_Sets,
    NewP_Assoc,
    SolP_Assoc,
    NewPO_Assoc,
    SolPO_Assoc
  ).

%! assert_node(
%!   +GraphAlignmentHash:atom,
%!   +Graph:atom,
%!   +GroupedBySharedPredicates:assoc,
%!   +GroupedBySharedProperties:assoc,
%!   +SharedPreds:ordset
%! ) is det.

assert_node(GA_Hash, G, Assoc, SharedPs, SharedPOs):-
  variant_sha1(GA_Hash-SharedPs, GAK_Hash),

  % Count the alignment pairs for the given sets of predicates.
  (
    assoc:get_assoc(SharedPs, Assoc, A_Sets)
  ;
    A_Sets = []
  ), !,
  aggregate(
    sum(A_SetSize),
    A_Set^(
      member(A_Set, A_Sets),
      length(A_Set, A_SetSize)
    ),
    NumberOfIdenticals
  ),

  % Check whether this identity node belongs to the lower or to the
  % higher approximation.
  % It belongs to the lower approximation if there is
  % at least one member that shares the given properties but does not
  % belong to the identity relation.
  (
    rdf_shares_properties(G, SharedPs, A_Sets, _X, _Y)
  ->
    InHigher = false
  ;
    InHigher = true,
    NumberOfEquivalents = NumberOfIdenticals
  ),

  % -- say it --
  assert(
    identity_node(
      GAK_Hash,
      GA_Hash,
      SharedPs,
      SharedPOs,
      InHigher,
      NumberOfIdenticals,
      NumberOfEquivalents
    )
  ).

%! clear_db is det.
% Clears the data store.

clear_db:-
  retractall(graph_alignment/7),
  retractall(identity_node/6).

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

%! rdf_shared(
%!   +Graph:atom,
%!   +Mode:oneof([p,po]),
%!   +Resources:ordset(iri),
%!   -SharedPredicates:ordset(iri),
%!   -SharedProperties:ordset(iri)
%! ) is det.
% Returns the predicates that both subjects possess.
%
% Moves from sets of resources to the shared properties of those resources.
%
% @see Wrapper around rdf_shared/7.

rdf_shared(G, Mode, Set, SolPs, SolPOs):-
  rdf_shared(G, Mode, Set, [], SolPs, [], SolPOs).

rdf_shared(G, Mode, [Res1|Resources], OldPs, SolPs, OldPOs, SolPOs):-
  % We assume a fully materialized graph.
  rdf(Res1, P, O, G),
  
  % If we were looking for predicates only,
  % the we can add the following restriction:
  (Mode == p -> \+ memberchk(P, OldPs) ; true),
  
  % All subject terms in the set must share the same
  % predicate-object pair / property (regardless of mode).
  forall(
    member(Res2, Resources),
    rdf(Res2, P, O, G)
  ), !,
  
  % Add a shared predicate.
  ord_add_element(OldPs, P, NewPs),
  
  % Mode-dependent inclusion of predicate-object pair / property
  (Mode = p -> NewPOs = OldPOs ; ord_add_element(OldPOs, P-O, NewPOs)),
  
  % Look for additional shared predicates (and properties).
  rdf_shared(G, Mode, [Res1|Resources], NewPs, SolPs, NewPOs, SolPOs).
rdf_shared(_G, _Mode, _Resources, SolPs, SolPs, SolPOs, SolPOs).

%! rdf_shares_properties(
%!   +Graph:atom,
%!   +SharedPreds:ordset(iri),
%!   +IdentitySet:ordset(iri),
%!   -SharedPreds1:iri,
%!   -SharedPreds2:iri
%! ) is det.
% Returns a single pair that shares the given predicates,
% but that does not belong to the given identity set.

rdf_shares_properties(G, Preds, A_Sets, X, Y):-
/*
  % We first order the predicates by probable occurrence.
  findall(
    Prob-Pred,
    (
      member(Pred, Preds),
      % We must first assure that something has been queried.
      % I have mailed JW about this.
      rdf_estimate_complexity(_, Pred, _, Prob)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, [P1|Ps]),
*/
  Preds = [P1|Ps],

  % Two resources at least share the first, i.e. least probable, property.
  rdf(X, P1, O, G),
  rdf(Y, P1, O, G),

  % We discarding symmetric results.
  X @< Y,

  % They are not in any of the identity sets.
  \+ ((
    member(A_Set, A_Sets),
    member(X, Y, A_Set)
  )),

  % They share all the other properties as well.
  forall(
    member(P2, Ps),
    (
      rdf(X, P2, O2, G),
      rdf(Y, P2, O2, G)
    )
  ).



% UPDATE %

%! predicates_to_sets(
%!   +Graph:atom,
%!   +Predicates:ordset(uri),
%!   -Sets:list(ordset(iri))
%! ) is det.
% Returns all pairs of resources that share all and only
% the given predicates.

predicates_to_sets(G, Ps, Sets):-
  findall(Set, predicates_to_set(G, Ps, Set), Sets).

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
  once(graph_alignment(GA_Hash,G,A_Sets,_,_,_,_)),
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

