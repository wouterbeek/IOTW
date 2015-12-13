:- module(
  inode,
  [
    clear_ihiers/0,
    create_ihier/3, % +Graph:rdf_graph
                    % +IdentitySets:list(ordset(iri))
                    % -IdentityHierarchyHash:atom
    ihier/5, % ?Graph:rdf_graph
             % ?IdentityHierarchyHash:atom
             % ?IdentitySets:list(ordset(iri))
             % ?GroupedBySharedPredicates:assoc
             % ?NumberOfAllIdentityPairs:nonneg
    inode/8 % ?NodeHash:atom
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
{〈{rdf:type}, {{ex:Andrea,ex:Wouter}, {ex:Amsterdam, ex:Berlin}}〉}
~~~

## Extending the identity relation

Possible extensions of the alignment pairs:
  1. Non-identity pairs in the lower-minus-higher approximation.
  2. Non-identity pairs in proper supersets of the lower approximation.

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(assoc_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_database)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).

:- use_module(iotw(iotw_generics)).

:- rdf_meta(create_ihier(r,+,-)).
:- rdf_meta(ihier(r,?,?,?,?)).

%! ihier(
%!   ?Graph:rdf_graph,
%!   ?IdentityHierarchyHash:atom,
%!   ?IdentitySets:list(ordset(iri)),
%!   ?GroupedBySharedPredicates:assoc,
%!   ?NumberOfAllIdentityPairs:nonneg
%! ) is nondet.

:- dynamic(ihier/5).

%! inode(
%!   ?NodeHash:atom,
%!   ?ParentHash:atom,
%!   ?SharedPredicates:ordset(or([iri,pair(iri)])),
%!   ?Approximation:oneof([higher,lower,none]),
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?IdentitySets:ordset(ordset(iri)),
%!   ?NumberOfPairs:nonneg,
%!   ?Pairs:ordset(pair(iri))
%! ) is nondet.

:- dynamic(inode/8).





%! create_ihier(
%!   +Graph:rdf_graph,
%!   +IdentitySets:list(ordset(iri)),
%!   -IdentityHierarchyHash:atom
%! ) is det.
% Asserts identity nodes for the given alignment sets
% into an identity hierarchy.
%
% ### Arguments
%
% @arg IdentitySets A collections of equivalence sets that represent
%      a given identity relation.
% @arg IdentityHierarchyHash The atomic hash of the
%      RDF graph + equivalence relation combination.
%
% ### Presuppositions
%
% This predicate make the following presuppositions:
%   1. The given RDF graph is fully materialized under RDFS and OWL regimes.
%   2. All typed literals are represented by their canonical lexical form.

create_ihier(G, ISets, IHierHash):-
  var(IHierHash), !,
  % We can identify this combination of
  % a graph and a collection of identity sets
  % later by using a hash.
  variant_sha1(G-ISets, IHierHash),
  create_ihier(G, ISets, IHierHash).
% The identity hierarchy with the given hash already exists.
create_ihier(_, _, IHierHash):-
  ihier(_, IHierHash, _, _, _), !.
% The identity hierarchy with the given hash must be constructed.
create_ihier(G, ISets, IHierHash):-
  % We need to establish the number of identity pairs based on
  % the collection of identity sets.
  %
  % For example, for every 2 pairs 〈x,y〉 and 〈y,z〉 we have
  % an identity set {x,y,z} representing 6 symmetric and transitive
  % pairs.  (We leave out the 3 reflexive pairs.)
  number_of_equivalence_pairs(ISets, NumberOfAllIPairs),

  % Assert the identity hierarchy based on the given identity sets.
  identity_sets_to_assoc(ISets, Assoc),

  % For every set of shared properties, we assert
  % an inode in the ihierarchy.
  assoc_to_keys(Assoc, SharedPs),
  maplist(create_inode(IHierHash, Assoc), SharedPs),

  assert(ihier(G, IHierHash, ISets, Assoc, NumberOfAllIPairs)).


%! identity_sets_to_assoc(
%!   +IdentitySets:list(ordset(iri)),
%!   -GroupedBySharedPredicates:assoc
%! ) is det.

identity_sets_to_assoc(ISets, Assoc):-
  empty_assoc(EmptyAssoc),
  identity_sets_to_assoc(ISets, EmptyAssoc, Assoc).


%! identity_sets_to_assoc(
%!   +IdentitySets:list(ordset(iri)),
%!   +OldGroupedBySharedPredicates:assoc,
%!   -NewGroupedBySharedPredicates:assoc
%! ) is det.

% No more identity sets. We are done!
identity_sets_to_assoc([], Assoc, Assoc):- !.
% For the next identity set ...
identity_sets_to_assoc([ISet|ISets], Assoc1, Assoc):-
  % Take the properties that the resources in the identity set share.
  rdf_shared(ISet, SharedPs),

  % Add the identity set as a value for the shared properties key.
  put_assoc_ord_member(SharedPs, Assoc1, ISet, Assoc2),

  identity_sets_to_assoc(ISets, Assoc2, Assoc).


%! create_inode(
%!   +IdentityHierarchyHash:atom,
%!   +GroupedBySharedPredicates:assoc,
%!   +SharedPreds:ordset
%! ) is det.
% The association lists record all sets of shared predicates
% and all sets of shared predicate-object pairs.

create_inode(IHierHash, Assoc, SharedPs):-
  get_assoc(SharedPs, Assoc, ISets),
  assert_inode0(IHierHash, SharedPs, ISets).


%! assert_inode0(
%!   +Hash:atom,
%!   +Shared:ordset,
%!   +ISets:list(ordset(iri))
%! ) is det.
% This works for both inodes (mode `p`) and isubnodes (mode `po`).

assert_inode0(Hash1, Shared, ISets):-
  variant_sha1(Hash1-Shared, Hash2),

  number_of_equivalence_pairs(ISets, NumberOfIPairs),

  % Check whether this identity node belongs to the lower or to the
  % higher approximation.
  % It belongs to the higher approximation if there is
  % at least one member that shares the given properties but does not
  % belong to the identity relation.
  check_shares(Shared, ISets),

  % Here we search the RDF graph for the number of subject term pairs
  % that share the given predicate or predicate-object pairs.
  % Only some of those pairs will be identity pairs.
  shared_predicates_to_pairs(Shared, Pairs),
  length(Pairs, NumberOfPairs),

  IPerc is NumberOfIPairs / NumberOfPairs,
  debug(inode, "IPerc: ~2f", [IPerc]),
  (IPerc =:= 1.0 -> Approx = lower ; Approx = higher),
  % -- say it --
  assert(
    inode(
      Hash2,
      Hash1,
      Shared,
      Approx,
      NumberOfIPairs,
      ISets,
      NumberOfPairs,
      Pairs
    )
  ).



%! check_shares(
%!   +SharedPredicates:ordset(iri),
%!   +IdentitySets:list(ordset(iri))
%! ) is det.
% Returns a single pair that shares the given predicates,
% but that does not belong to the given identity set.
%
% @arg IdentitySets Only the identity sets formed by resources
%        that share the given predicates.

check_shares(SharedPs, ISets):-
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
  rdf(X, P1, O1),
  rdf(Y, P1, O1),

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
      rdf(X, P2, O2),
      rdf(Y, P2, O2)
    )
  ), !.


%! clear_ihiers is det.
% Clears all current identity hierarchies.

clear_ihiers:-
  retractall(ihier(_, _, _, _, _)),
  retractall(inode(_, _, _, _, _, _, _, _)).


%! rdf_shared(+Resources:ordset(iri), -SharedPredicates:ordset(iri)) is det.
% Returns the properties that all the given resources share.
%
% Moves from sets of resources to the shared properties of those resources.

rdf_shared(Set, Ps):-
  rdf_shared(Set, [], Ps).

rdf_shared([S1|Ss], Ps1, Ps):-
  if_debug(inode, rdf_print_describe(S1, _, [])),

  % We assume a fully materialized graph.
  rdf(S1, P, O),

  % Depending on the granularity mode
  % (i.e., predicate-object pairs or only predicates),
  % we enforce a different restriction on existing results.
  \+ memberchk(P, Ps1),

  % All resources in the identity set must share the same
  % predicate-object pair (regardless of mode).
  % Here we assume that the typed literals are represented
  % with their canonical lexical form.
  forall(member(S2, Ss), rdf(S2, P, O)),

  % Add a shared predicate term.
  ord_add_element(Ps1, P, Ps2),

  % Look for additional shared predicate terms or predicate-object term pairs.
  rdf_shared([S1|Ss], Ps2, Ps).
% No shared p or po could be found anymore.
rdf_shared(_, Ps, Ps).



%! shared_predicates_to_pairs(
%!   +Predicates:list(iri),
%!   -Pairs:list(pair(or([bnode,iri])))
%! ) is det.

shared_predicates_to_pairs([P1|Ps], Pairs):-
  aggregate_all(
    set(X-Y),
    (
      % We are looking for subject terms that share the same predicates...
      rdf(X, P1, O1),
      rdf(Y, P1, O1),

      % Efficiency?
      %X @< Y,

      % All predicates are shared.
      forall(member(P2, Ps), (rdf(X, P2, O2), rdf(Y, P2, O2)))
    ),
    Pairs
  ).
