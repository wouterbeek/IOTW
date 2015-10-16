:- module(
  inode,
  [
    clear_ihiers/0,
    create_ihier/4, % +Graph:atom
                    % +IdentitySets:list(ordset(iri))
                    % -IdentityHierarchyHash:atom
                    % +Options:list(compound)
    ihier/6, % ?IdentityHierarchyHash:atom
             % ?Graph:atom
             % ?IdentitySets:list(ordset(iri))
             % ?GroupedBySharedPredicates:assoc
             % ?GroupedBySharedPredicateObjectPairs:assoc
             % ?NumberOfAllIdentityPairs:nonneg
    inode/9 % ?Mode:oneof([p,po])
            % ?NodeHash:atom
            % ?ParentHash:atom
            % ?Shared:ordset(or([iri,pair(iri)]))
            % ?Approximation:oneof([higher,lower,none])
            % ?NumberOfIdentityPairs:nonneg
            % ?IdentitySets:orset(ordset(iri))
            % ?NumberOfPairs:nonneg
            % ?Pairs:orset(pair(iri))
  ]
).

/** <module> Identity nodes

We use the following example:

〈ex:Andrea,    rdf:type, foaf:Person   〉
〈ex:Wouter,    rdf:type, foaf:Person   〉
〈ex:Amsterdam, rdf:type, ex:Capital    〉
〈ex:Amsterdam, rdf:type, ex:City       〉
〈ex:Amsterdam, rdf:type, ex:GeoLocation〉
〈ex:Berlin   , rdf:type, ex:Capital    〉
〈ex:Berlin   , rdf:type, ex:City       〉
〈ex:Berlin   , rdf:type, ex:GeoLocation〉

Shared predicates are stored in an association list, called `P_Assoc`,
which maps sets of predicate terms to sets of resources that share those
predicates for identical object terms.

For the example given above:
~~~
{〈{rdf:type}, {{ex:Andrea,ex:Wouter}, {ex:Amsterdam, ex:Berline}}〉}
~~~

Shared predicate-object pairs are stored in an association list,
called `PO_Assoc`, which maps sets of predicate terms to maps from
sets of object terms to resources.

For the example above:
~~~
{〈{rdf:type},
  {〈{foaf:Person}, {ex:Andrea,ex:Wouter}〉,
   〈{ex:Capital,ex:City,ex:GeoLocation}, {ex:Amsterdam,ex:Berlin}〉}〉}
~~~

## Extending the identity relation

Possible extensions of the alignment pairs:
  1. Non-identity pairs in the lower-minus-higher approximation.
  2. Non-identity pairs in proper supersets of the lower approximation.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(assoc_ext)).
:- use_module(library(debug)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

:- use_module(iotw_generics).

%! ihier(
%!   ?IdentityHierarchyHash:atom,
%!   ?Graph:atom,
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
%!   ?SharedPredicates:ordset(or([iri,pair(iri)])),
%!   ?Approximation:oneof([higher,lower,none]),
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?IdentitySets:ordset(ordset(iri)),
%!   ?NumberOfPairs:nonneg,
%!   ?Pairs:ordset(pair(iri))
%! ) is nondet.

:- dynamic(inode/9).

:- predicate_options(create_ihier/4, 4, [
     granularity(+oneof([higher,lower,none]))
   ]).





%! create_ihier(
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   -IdentityHierarchyHash:atom,
%!   +Options:list(compound)
%! ) is det.
% Asserts identity nodes for the given alignment sets
% into an identity hierarchy.
%
% ### Arguments
%
% @arg Graph The atomic name of an RDF graph.
% @arg IdentitySets A collections of equivalence sets that represent
%      a given identity relation.
% @arg IdentityHierarchyHash The atomic hash of the
%      RDF graph + equivalence relation combination.
% @arg Options A list of name-value pairs.
%
% ### Options
%
% The following options are supported:
%   * granularity(+oneof([higher,lower,none]))
%
% ### Presuppositions
%
% This predicate make the following presuppositions:
%   1. The given RDF graph is fully materialized under RDFS and OWL regimes.
%   2. All typed literals are represented by their canonical lexical form.

create_ihier(G, ISets, IHierHash, Opts):-
  var(IHierHash), !,
  % We can identify this combination of
  % a graph and a collection of identity sets
  % later by using a hash.
  variant_sha1(G-ISets, IHierHash),
  create_ihier(G, ISets, IHierHash, Opts).
% The identity hierarchy with the given hash already exists.
create_ihier(_, _, IHierHash, _):-
  ihier(IHierHash, _, _, _, _, _), !.
% The identity hierarchy with the given hash must be constructed.
create_ihier(G, ISets, IHierHash, Opts):-
  % We need to establish the number of identity pairs based on
  % the collection of identity sets.
  %
  % For example, for every 2 pairs 〈x,y〉 and 〈y,z〉 we have
  % an identity set {x,y,z} representing 6 symmetric and transitive
  % pairs.  (We leave out the 3 reflexive pairs.)
  number_of_equivalence_pairs(ISets, NumberOfAllIPairs),

  % Make sure the granularity mode is set.
  option(granularity(Mode), Opts, p),

  % Assert the identity hierarchy based on the given identity sets.
  identity_sets_to_assocs(Mode, G, ISets, P_Assoc, PPO_Assoc),

  % For every set of shared properties, we assert
  % an inode in the ihierarchy.
  assoc_to_keys(P_Assoc, SharedPs),
  maplist(create_inode(Mode, IHierHash, G, P_Assoc, PPO_Assoc), SharedPs),

  assert(ihier(IHierHash, G, ISets, P_Assoc, PPO_Assoc, NumberOfAllIPairs)).


%! identity_sets_to_assocs(
%!   +Mode:oneof([p,po]),
%!   +Graph:atom,
%!   +IdentitySets:list(ordset(iri)),
%!   -GroupedBySharedPredicates:assoc,
%!   -GroupedBySharedPredicateObjectPairs:assoc
%! ) is det.
% @see Wrapper around identity_sets_to_assocs/7,
%      adding empty association lists to store the `p` and `po` data in.

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

% No more identity sets. We are done!
identity_sets_to_assocs(
  _,
  _,
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
  % Take the properties that the resources in the identity set share.
  rdf_shared(G, Mode, ISet, SharedPs, SharedPOs),

  % Add the identity set as a value for the shared properties key.
  put_assoc_ord_member(SharedPs, P_Assoc1, ISet, P_Assoc2),

  % Add the identity set as a value to the shared objects key of
  % the association list that is the value of the shared predicates key.

  % (1/3) If the shared predicates have an entry in
  % the `po` association list, then this entry is reused.
  % Otherwise a new association list is created.
  (   % Get the nested assoc.
      get_assoc(SharedPs, PPO_Assoc1, PO_Assoc1)
  ->  true
  ;   empty_assoc(PO_Assoc1)
  ),

  % (2/3) Add the identity set as a value for
  % the shared predicate-object pairs key.
  put_assoc_ord_member(SharedPOs, PO_Assoc1, ISet, PO_Assoc2),

  % (3/3) Now we must REPLACE the nested association list.
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


%! create_inode(
%!   +Mode:oneof([p,po]),
%!   +IdentityHierarchyHash:atom,
%!   +Graph:atom,
%!   +GroupedBySharedPredicates:assoc,
%!   +GroupedBySharedPredicateObjectPairs:assoc,
%!   +SharedPreds:ordset
%! ) is det.
% The association lists record all sets of shared predicates
% and all sets of shared predicate-object pairs.

create_inode(Mode, IHierHash, G, P_Assoc, PPO_Assoc, SharedPs):-
  (   % Skip the assertion of isubnodes.
      Mode == p
  ->  true
  ;   % We need to identify the isubnodes as belonging to the proper inode.
      variant_sha1(IHierHash-SharedPs, INodeHash),

      % Retrieve the association list from sets of predicate-object pairs
      % to sets of resources.
      get_assoc(SharedPs, PPO_Assoc, PO_Assoc),

      % For each key in the association list we add an isubnode.
      assoc_to_list(PO_Assoc, Pairs),
      forall(
        member(SharedPOs-ISets1, Pairs),
        assert_inode0(po, G, INodeHash, SharedPOs, ISets1)
      )
  ),
  get_assoc(SharedPs, P_Assoc, ISets2),
  assert_inode0(p, G, IHierHash, SharedPs, ISets2).


%! assert_inode0(
%!   +Mode:oneof([p,po]),
%!   +Graph:atom,
%!   +Hash:atom,
%!   +Shared:ordset,
%!   +ISets:list(ordset(iri))
%! ) is det.
% This works for both inodes (mode `p`) and isubnodes (mode `po`).
%
% @tbd Add alpha lower and alpha higher as options.
% @tbd For alpha lower = 0.0 and alpha higher = 1.0 we can use
%      the check_shares_... predicates for efficiency.

assert_inode0(Mode, G, Hash1, Shared, ISets):-
  variant_sha1(Hash1-Shared, Hash2),

  number_of_equivalence_pairs(ISets, NumberOfIPairs),

/*
  % Check whether this identity node belongs to the lower or to the
  % higher approximation.
  % It belongs to the higher approximation if there is
  % at least one member that shares the given properties but does not
  % belong to the identity relation.
  check_shares(Mode, Graph, Shared, ISets)
*/
  
  % Here we search the RDF graph for the number of subject term pairs
  % that share the given predicate or predicate-object pairs.
  % Only some of those pairs will be identity pairs.
  shared_predicates_to_pairs(G, Shared, Pairs),
  length(Pairs, NumberOfPairs),
  
  IPerc is NumberOfIPairs / NumberOfPairs,
  debug(inode, "IPerc: ~2f", [IPerc]),
  (   % Alpha lower
      IPerc >= 0.95
  ->  Approx = lower
  ;   % Alpha higher
      IPerc > 0.05
  ->  Approx = higher
  ;   Approx = none
  ),

  (   Approx == none
  ->  true
  ;   % -- say it --
      assert(
        inode(
          Mode,
          Hash2,
          Hash1,
          Shared,
          Approx,
          NumberOfIPairs,
          ISets,
          NumberOfPairs,
          Pairs
        )
      )
  ).



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
% @arg IdentitySets Only the identity sets formed by resources
%        that share the given predicates.

check_shares_predicates(G, SharedPs, ISets):-
  % We first order the predicates by probable occurrence.
  findall(
    Prob-Pred,
    (
      member(Pred, SharedPs),
      % We must first assure that something has been queried.
      % I have mailed JW about this.
      rdf_estimate_complexity(_, Pred, _, Prob)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, [P1|Ps]),

  % Two resources at least share the first, i.e. least probable, property.
  rdf(X, P1, O1, G),
  rdf(Y, P1, O1, G),

  % We discard symmetric results.
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


%! clear_ihiers is det.
% Clears all current identity hierarchies.

clear_ihiers:-
  retractall(ihier(_, _, _, _, _, _)),
  retractall(inode(_, _, _, _, _, _, _, _, _)).


%! rdf_shared(
%!   +Graph:atom,
%!   +Mode:oneof([p,po]),
%!   +Resources:ordset(iri),
%!   -SharedPredicates:ordset(iri),
%!   -SharedProperties:ordset(iri)
%! ) is det.
% Returns the properties that all the given resources share.
%
% Moves from sets of resources to the shared properties of those resources.

rdf_shared(G, Mode, Set, Ps, POs):-
  rdf_shared(G, Mode, Set, [], Ps, [], POs).

rdf_shared(G, Mode, [S1|Ss], Ps1, Ps, POs1, POs):-
  % We assume a fully materialized graph.
  rdf(S1, P, O, G),

  % Depending on the granularity mode
  % (i.e., predicate-object pairs or only predicates),
  % we enforce a different restriction on existing results.
  (Mode == p -> \+ memberchk(P, Ps1) ; \+ memberchk(P-O, POs1)),

  % All resources in the identity set must share the same
  % predicate-object pair (regardless of mode).
  % Here we assume that the typed literals are represented
  % with their canonical lexical form.
  forall(member(S2, Ss), rdf(S2, P, O, G)),

  % Add a shared predicate term.
  ord_add_element(Ps1, P, Ps2),

  % Mode-dependent inclusion of predicate-object term pairs.
  (Mode == p -> POs2 = POs1 ; ord_add_element(POs1, P-O, POs2)),

  % Look for additional shared predicate terms or predicate-object term pairs.
  rdf_shared(G, Mode, [S1|Ss], Ps2, Ps, POs2, POs).
% No shared p or po could be found anymore.
rdf_shared(_, _, _, Ps, Ps, POs, POs).



%! shared_predicates_to_pairs(
%!   +Graph:atom,
%!   +Predicates:list(iri),
%!   -Pairs:list(pair(or([bnode,iri])))
%! ) is det.

shared_predicates_to_pairs(G, [P1|Ps], Pairs):-
  aggregate_all(
    set(X-Y),
    (
      % We are looking for subject terms that share the same predicates...
      rdf(X, P1, O1, G),
      rdf(Y, P1, O1, G),
      
      % Efficiency?
      %X @< Y,
      
      % All predicates are shared.
      forall(
        member(P2, Ps),
        (
          rdf(X, P2, O2, G),
          rdf(Y, P2, O2, G)
        )
      )
      
      /*
      This is wrong, e.g., it could exclude identity pairs.
      % There is no predicate that is not shared.
      \+ ((
        rdf(X, Pn, On, Graph),
        rdf(Y, Pn, On, Graph),
        \+ member(Pn, [P1|Ps])
      ))
      */
    ),
    Pairs
  ).
