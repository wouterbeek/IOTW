:- module(
  inode,
  [
    assert_inodes/4, % +Options:list(nvpair)
                     % +Graph:atom
                     % +IdentitySets:list(ordset(iri))
                     % -IdentityHierarchyHash:atom
    ihier/6, % ?IdentityHierarchyHash:atom
             % ?RDF_Graph:atom
             % ?IdentitySets:list(ordset(iri))
             % ?GroupedBySharedPredicates:assoc
             % ?GroupedBySharedPredicateObjectPairs:assoc
             % ?NumberOfAllIdentityPairs:nonneg
    inode/7 % ?Mode:oneof([p,po])
            % ?NodeHash:atom
            % ?ParentHash:atom
            % ?Shared:ordset(or([iri,pair(iri)]))
            % ?InHigherApproximation:boolean
            % ?NumberOfIdentityPairs:nonneg
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
@version 2013/05, 2013/08-2013/10
*/

:- use_module(generics(assoc_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(ordset_ext)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_term)). % Used in meta-options.

%! ihier(
%!   ?IdentityHierarchyHash:atom,
%!   ?RDF_Graph:atom,
%!   ?IdentitySets:list(ordset(iri)),
%!   ?GroupedBySharedPredicates:assoc,
%!   ?GroupedBySharedPredicateObjectPairs:assoc,
%!   ?NumberOfAllIdentityPairs:nonneg
%! ) is nondet.

:- dynamic(ihier/6).

%! inode(
%!   ?Mode:oneof([p,po]),
%!   ?NodeHash:atom,
%!   ?ParentHash:atom,
%!   ?Shared:ordset(or([iri,pair(iri)])),
%!   ?InHigherApproximation:boolean,
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?NumberOfPairs:nonneg
%! ) is nondet.

:- dynamic(inode/7).



%! assert_inodes(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   -IdentityHierarchyHash:atom
%! ) is det.
% Asserts identity nodes for the given alignment sets.
%
% @param Options A list of name-value pairs.
% @param Graph The atomic name of an RDF graph.
% @param IdentitySets A collections of equivalence sets that represent
%        a given identity relation.
% @param IdentityHierarchyHash The atomic hash of the
%        RDF graph + equivalence relation combination.

assert_inodes(O, G, ISets, IHierHash):-
  % We need to establish the number of identity pairs based on
  % the collection of identity sets, because this can be larger
  % that the number of pairs.
  % For every two pairs `X-Y` and `Y-Z` we have an identity set ${X,Y,Z}$
  % representing three non-reflexive and non-symmetric identity pairs.
  equivalence_sets_to_number_of_equivalence_pairs(ISets, NumberOfAllIdPairs),

  clear_db,
  option(granularity(Mode), O, p),

  % We can identify this RDF graph and alignment pairs combination later
  % using a hash.
  variant_sha1(G-ISets, IHierHash),

  % Assert the identity hierarchy based on the given identity sets.
  identity_sets_to_assocs(Mode, G, ISets, P_Assoc, PPO_Assoc),
  assoc_to_keys(P_Assoc, SharedPs),
  maplist(assert_node(Mode, IHierHash, G, P_Assoc, PPO_Assoc), SharedPs),

  assert(
    ihier(
      IHierHash,
      G,
      ISets,
      P_Assoc,
      PPO_Assoc,
      NumberOfAllIdPairs
    )
  ).

%! identity_sets_to_assocs(
%!   +Mode:oneof([p,po]),
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   -GroupedBySharedPredicates:assoc,
%!   -GroupedBySharedPredicateObjectPairs:assoc
%! ) is det.
% @see See identity_sets_to_assocs/7 where the real work happens.

identity_sets_to_assocs(Mode, G, ISets, P_Assoc, PPO_Assoc):-
  empty_assoc(EmptyP_Assoc),
  empty_assoc(EmptyPPO_Assoc),
  identity_sets_to_assocs(
    Mode,
    G,
    ISets,
    EmptyP_Assoc,
    P_Assoc,
    EmptyPPO_Assoc,
    PPO_Assoc
  ).

%! identity_sets_to_assocs(
%!   +Mode:oneof([p,po]),
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   +OldGroupedBySharedPredicates:assoc,
%!   -NewGroupedBySharedPredicates:assoc,
%!   +OldGroupedBySharedPredicateObjectPairs:assoc,
%!   -NewGroupedBySharedPredicateObjectPairs:assoc
%! ) is det.

% No more identity sets. Done!
identity_sets_to_assocs(
  _Mode,
  _G,
  [],
  SolP_Assoc,
  SolP_Assoc,
  SolPPO_Assoc,
  SolPPO_Assoc
):- !.
% For the next identity set ...
identity_sets_to_assocs(
  Mode,
  G,
  [ISet|ISets],
  P_Assoc1,
  P_Assoc3,
  PPO_Assoc1,
  PPO_Assoc3
):-
  % Take the predicates that the resources in the identity set share.
  rdf_shared(G, Mode, ISet, SharedPs, SharedPOs),

  % Add to the ordered set under the `SharedPs` key.
  put_assoc_ord_member(SharedPs, P_Assoc1, ISet, P_Assoc2),

  % Add the alignment pair as a value to the shared objects key of the
  % association list that is a value to the shared predicates key.
  (
    % Get the nested assoc.
    get_assoc(SharedPs, PPO_Assoc1, PO_Assoc1), !
  ;
    empty_assoc(PO_Assoc1)
  ),
  % Add to the ordered set under the `SharedPOs` key of the nested assoc.
  put_assoc_ord_member(SharedPOs, PO_Assoc1, ISet, PO_Assoc2),
  % REPLACE the nested assoc.
  put_assoc(SharedPs, PPO_Assoc1, PO_Assoc2, PPO_Assoc2),

  identity_sets_to_assocs(
    Mode,
    G,
    ISets,
    P_Assoc2,
    P_Assoc3,
    PPO_Assoc2,
    PPO_Assoc3
  ).

%! assert_node(
%!   +Mode:oneof([p,po]),
%!   +IdentityHierarchyHash:atom,
%!   +Graph:atom,
%!   +GroupedBySharedPredicates:assoc,
%!   +GroupedBySharedProperties:assoc,
%!   +SharedPreds:ordset
%! ) is det.

assert_node(Mode, IHierHash, G, P_Assoc, PPO_Assoc, SharedPs):-
  (
    % Skip the assertion of isubnodes.
    Mode == p, !
  ;
    % We need to identify the isubnodes as belonging to the proper inode.
    variant_sha1(IHierHash-SharedPs, INodeHash),

    % Retrieve the association list from sets of predicate-object pairs
    % to sets of resources.
    get_assoc(SharedPs, PPO_Assoc, PO_Assoc),

    % For each key in the association list we add a isubnode.
    assoc_to_list(PO_Assoc, Pairs),
    forall(
      member(SharedPOs-IdSets1, Pairs),
      assert_node_(po, G, INodeHash, SharedPOs, IdSets1)
    )
  ),

  get_assoc(SharedPs, P_Assoc, IdSets2),
  assert_node_(p, G, IHierHash, SharedPs, IdSets2).

%! assert_node_(
%!   +Mode:oneof([p,po]),
%!   +Graph:atom,
%!   +Hash:atom,
%!   +Shared:ordset,
%!   +ISets:list(ordset(iri))
%! ) is det.
% This works for both inodes (mode `p`) and isubnodes (mode `po`).

assert_node_(Mode, G, Hash1, Shared, ISets):-
  variant_sha1(Hash1-Shared, Hash2),
  equivalence_sets_to_number_of_equivalence_pairs(ISets, NumberOfIPairs),

  % Check whether this identity node belongs to the lower or to the
  % higher approximation.
  % It belongs to the lower approximation if there is
  % at least one member that shares the given properties but does not
  % belong to the identity relation.
  (
    check_shares(Mode, G, Shared, ISets)
  ->
    InHigher = false
  ;
    InHigher = true,
    NumberOfPairs = NumberOfIPairs
  ),

  % -- say it --
  assert(
    inode(Mode,Hash2,Hash1,Shared,InHigher,NumberOfIPairs,NumberOfPairs)
  ).

%! clear_db is det.
% Clears the data store.

clear_db:-
  retractall(ihier(_,_,_,_,_,_)),
  retractall(inode(_,_,_,_,_,_,_)).

%! rdf_shared(
%!   +Graph:atom,
%!   +Mode:oneof([p,po]),
%!   +Resources:ordset(iri),
%!   -SharedPredicates:ordset(iri),
%!   -SharedProperties:ordset(iri)
%! ) is det.
% Returns the predicates that all the given resources possess.
%
% Moves from sets of resources to the shared properties of those resources.
%
% @see See rdf_shared/7 for the real work.

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
  % Here we assume that all typed literals are using
  % their canonical lexical form.
  forall(member(Res2, Resources), rdf(Res2, P, O, G)), !,

  % Add a shared predicate.
  ord_add_element(OldPs, P, NewPs),

  % Mode-dependent inclusion of predicate-object pair / property
  (Mode = p -> NewPOs = OldPOs ; ord_add_element(OldPOs, P-O, NewPOs)),

  % Look for additional shared predicates (and properties).
  rdf_shared(G, Mode, [Res1|Resources], NewPs, SolPs, NewPOs, SolPOs).
rdf_shared(_G, _Mode, _Resources, SolPs, SolPs, SolPOs, SolPOs).

check_shares(p, G, SharedPs, ISets):- !,
  check_shares_predicates(G, SharedPs, ISets).
check_shares(po, G, SharedPOs, ISets):-
  check_shares_predicate_object_pairs(G, SharedPOs, ISets).

check_shares_predicate_object_pairs(G, [P1-O1|POs], ISets):-
  % Two resources at least share the first, i.e. least probable, property.
  rdf(X, P1, O1, G),
  rdf(Y, P1, O1, G),

  % We discarding symmetric results.
  X @< Y,

  % They are not in any of the identity sets.
  \+ ((
    member(ISet, ISets),
    member(X, Y, ISet)
  )),

  % They share all the other properties as well.
  forall(
    member(P2-O2, POs),
    (
      rdf(X, P2, O2, G),
      rdf(Y, P2, O2, G)
    )
  ), !.

%! check_shares_predicates(
%!   +Graph:atom,
%!   +SharedPredicates:ordset(iri),
%!   +IdentitySets:list(ordset(iri))
%! ) is det.
% Returns a single pair that shares the given predicates,
% but that does not belong to the given identity set.
%
% @param IdentitySets Only the identity sets formed by resources
%        that share the given predicates.

check_shares_predicates(G, SharedPs, ISets):-
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
    member(ISet, ISets),
    member(X, Y, ISet)
  )),

  % They share all the other properties as well.
  forall(
    member(P2, Ps),
    (
      rdf(X, P2, O2, G),
      rdf(Y, P2, O2, G)
    )
  ), !.

