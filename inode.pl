:- module(
  inodes,
  [
    assert_identity_nodes/4, % +Options:list(nvpair)
                             % +Graph:atom
                             % +IdentitySets:list(ordset(iri))
                             % -IdentityHierarchyHash:atom
    identity_hierarchy/7, % ?IdentityHierarchyHash:atom
                          % ?RDF_Graph:atom
                          % ?IdentitySets:list(ordset(iri))
                          % ?GroupedBySharedPredicates:assoc
                          % ?GroupedBySharedPredicateObjectPairs:assoc
                          % ?NumberOfIdentitySets:nonneg
                          % ?NumberOfPairs:nonneg
    identity_node/6 % ?IdentityNodeHash:atom
                    % ?IdentityHierarchyHash:atom
                    % ?SharedPredicates:ordset(iri)
                    % ?InHigherApproximation:boolean
                    % ?NumberOfIdentitySets:nonneg
                    % ?NumberOfPairs:nonneg
  ]
).

/** <module> Identity nodes

We use the following example:
~~~
<ex:Andrea,    rdf:type, foaf:Person   >
<ex:Wouter,    rdf:type, foaf:Person   >
<ex:Amsterdam, rdf:type, ex:Capital    >
<ex:Amsterdam, rdf:type, ex:City       >
<ex:Amsterdam, rdf:type, ex:GeoLocation>
<ex:Berlin   , rdf:type, ex:Capital    >
<ex:Berlin   , rdf:type, ex:City       >
<ex:Berlin   , rdf:type, ex:GeoLocation>
~~~

Shared predicates are stored in an association list, called `P_Assoc`,
which maps sets of predicate terms to sets of resources that share those
predicates for identical object terms.

For the example given above:
~~~
{<{rdf:type}, {{ex:Andrea,ex:Wouter}, {ex:Amsterdam, ex:Berline}}>}
~~~

Shared predicate-object pairs are stored in an association list,
called `PO_Assoc`, which maps sets of predicate terms to maps from
sets of object terms to resources.

For the example above:
~~~
{<{rdf:type},
  {<{foaf:Person}, {ex:Andrea,ex:Wouter}>,
   <{ex:Capital,ex:City,ex:GeoLocation}, {ex:Amsterdam,ex:Berlin}>}>}
~~~

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

%! identity_hierarchy(
%!   ?IdentityHierarchyHash:atom,
%!   ?RDF_Graph:atom,
%!   ?IdentitySets:list(ordset(iri)),
%!   ?GroupedBySharedPredicates:assoc,
%!   ?GroupedBySharedPredicateObjectPairs:assoc,
%!   ?NumberOfIdentitySets:nonneg,
%!   ?NumberOfPairs:nonneg
%! ) is nondet.

:- dynamic(identity_hierarchy/7).

%! identity_node(
%!   ?IdentityNodeHash:atom,
%!   ?IdentityHierarchyHash:atom,
%!   ?SharedPredicates:ordset(iri),
%!   ?InHigherApproximation:boolean,
%!   ?NumberOfIdentitySets:nonneg,
%!   ?NumberOfPairs:nonneg
%! ) is nondet.

:- dynamic(identity_node/6).



%! assert_identity_nodes(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   -IdentityHierarchyHash:atom
%! ) is det.
% Asserts identity nodes for the given alignment sets.

assert_identity_nodes(O, G, IdSets, IHierHash):-
  clear_db,

  % We can identify this RDF graph and alignment pairs combination later
  % using a hash.
  variant_sha1(G-IdSets, IHierHash),

  % Calculate the number of identity sets.
  length(IdSets, NumberOfIdentitySets),

  % Calculate the number of pairs that can be formed out of subject terms
  % that are not blank nodes.
  setoff(S, (rdf_subject(G, S), \+ rdf_is_bnode(S)), Ss),
  length(Ss, NumberOfS),
  NumberOfPairs is NumberOfS ** 2,

  % Assert the identity hierarchy based on the given identity sets.
  identity_sets_to_assocs(O, G, IdSets, P_Assoc, PPO_Assoc),
  assoc_to_keys(P_Assoc, SharedPs),
  maplist(assert_node(IHierHash, G, P_Assoc, PPO_Assoc), SharedPs),

  assert(
    identity_hierarchy(
      IHierHash,
      G,
      IdSets,
      P_Assoc,
      PPO_Assoc,
      NumberOfIdentitySets,
      NumberOfPairs
    )
  ).

%! identity_sets_to_assocs(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   -GroupedBySharedPredicates:assoc,
%!   -GroupedBySharedPredicateObjectPairs:assoc
%! ) is det.
% @see Wrapper around identity_sets_to_assocs/7.

identity_sets_to_assocs(O, G, IdSets, P_Assoc, PPO_Assoc):-
  empty_assoc(EmptyP_Assoc),
  empty_assoc(EmptyPO_Assoc),
  option(granularity(Mode), O, p),
  identity_sets_to_assocs(
    G,
    Mode,
    IdSets,
    EmptyP_Assoc,
    P_Assoc,
    EmptyPO_Assoc,
    PPO_Assoc
  ).

%! identity_sets_to_assocs(
%!   +Graph:atom,
%!   +Mode:oneof([p,po]),
%!   +IdentitySets:list(ordset(iri)),
%!   +OldGroupedBySharedPredicates:assoc,
%!   -NewGroupedBySharedPredicates:assoc,
%!   +OldGroupedBySharedPredicateObjectPairs:assoc,
%!   -NewGroupedBySharedPredicateObjectPairs:assoc
%! ) is det.

identity_sets_to_assocs(
  _G,
  _Mode,
  [],
  SolP_Assoc,
  SolP_Assoc,
  SolPPO_Assoc,
  SolPPO_Assoc
):- !.
identity_sets_to_assocs(
  G,
  Mode,
  [IdSet|IdSets],
  P_Assoc1,
  P_Assoc3,
  PPO_Assoc1,
  PPO_Assoc3
):-
  % Take the predicates that the alignment pair shares.
  rdf_shared(G, Mode, IdSet, SharedPs, SharedPOs),

  % Add the alignment pair as a value to the shared predicates key.
  put_assoc(SharedPs, P_Assoc1, IdSet, P_Assoc2),

  % Add the alignment pair as a value to the shared objects key of the
  % association list that is a value to the shared predicates key.
  (
    get_assoc(SharedPs, PPO_Assoc1, PO_Assoc1), !
  ;
    empty_assoc(PO_Assoc1)
  ),
  put_assoc(SharedPOs, PO_Assoc1, IdSet, PO_Assoc2),
  % This predicate does both insertion and change/update.
  put_assoc(SharedPs, PPO_Assoc1, PO_Assoc2, PPO_Assoc2),

  identity_sets_to_assocs(
    G,
    Mode,
    IdSets,
    P_Assoc2,
    P_Assoc3,
    PPO_Assoc2,
    PPO_Assoc3
  ).

%! assert_node(
%!   +IdentityHierarchyHash:atom,
%!   +Graph:atom,
%!   +GroupedBySharedPredicates:assoc,
%!   +GroupedBySharedProperties:assoc,
%!   +SharedPreds:ordset
%! ) is det.

assert_node(IHierHash, G, P_Assoc, _PPO_Assoc, SharedPs):-
  variant_sha1(IHierHash-SharedPs, INodeHash),

  % Count the alignment pairs for the given sets of predicates.
  (
    assoc:get_assoc(SharedPs, P_Assoc, IdSets)
  ;
    IdSets = []
  ), !,
  aggregate(
    sum(A_SetSize),
    IdSet^(
      member(IdSet, IdSets),
      length(IdSet, A_SetSize)
    ),
    NumberOfIdenticals
  ),

  % Check whether this identity node belongs to the lower or to the
  % higher approximation.
  % It belongs to the lower approximation if there is
  % at least one member that shares the given properties but does not
  % belong to the identity relation.
  (
    rdf_shares_properties(G, SharedPs, IdSets, _X, _Y)
  ->
    InHigher = false
  ;
    InHigher = true,
    NumberOfEquivalents = NumberOfIdenticals
  ),

  % -- say it --
  assert(
    identity_node(
      INodeHash,
      IHierHash,
      SharedPs,
      InHigher,
      NumberOfIdenticals,
      NumberOfEquivalents
    )
  ).

%! clear_db is det.
% Clears the data store.

clear_db:-
  retractall(identity_hierarchy/7),
  retractall(identity_node/6).

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
  (Mode == p -> \+ memberchk(P, OldPs) ; \+ memberchk(P-O, OldPOs)),

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
%!   +IdentitySets:list(ordset(iri)),
%!   -SharedPreds1:iri,
%!   -SharedPreds2:iri
%! ) is det.
% Returns a single pair that shares the given predicates,
% but that does not belong to the given identity set.
%
% @param IdentitySets Only the identity sets formed by resources
%        that share the given predicates.

rdf_shares_properties(G, SharedPs, IdSets, X, Y):-
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
  SharedPs = [P1|Ps],

  % Two resources at least share the first, i.e. least probable, property.
  rdf(X, P1, O, G),
  rdf(Y, P1, O, G),

  % We discarding symmetric results.
  X @< Y,

  % They are not in any of the identity sets.
  \+ ((
    member(IdSet, IdSets),
    member(X, Y, IdSet)
  )),

  % They share all the other properties as well.
  forall(
    member(P2, Ps),
    (
      rdf(X, P2, O2, G),
      rdf(Y, P2, O2, G)
    )
  ).

