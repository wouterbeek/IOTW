:- module(
  inode_update,
  [
    update_identity_node/1 % +INodeHash:atom
  ]
).

/** <module> Updates for identity nodes

@author Wouter Beek
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(iotw(inode)).
:- use_module(library(aggregate)).
%:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

%:- debug(inode_update).



%! predicates_to_set(
%!   +Graph:atom,
%!   +SharedPredicates:ordset(iri),
%!   -SimilarResources:ordset(iri)
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
  findall(
    Set,
    predicates_to_set(G, Ps, Set),
    Sets
  ).

update_identity_node(INodeHash):-
  once(
    inode(
      Mode,
      INodeHash,
      IHierHash,
      SharedPs,
      InHigher,
      NumberOfIdPairs,
      NumberOfPairs1
    )
  ),
  once(ihier(IHierHash,G,IdSets,_,_,_)),
  var(NumberOfPairs1), !,
  predicates_to_sets(G, IdSets, Sets),
  aggregate_all(
    sum(Length),
    (
      member(Set, Sets),
      length(Set, Length)
    ),
    NumberOfPairs2
  ),
  db_replace_novel(
    inode(
      Mode,
      INodeHash,
      IHierHash,
      SharedPs,
      InHigher,
      NumberOfIdPairs,
      NumberOfPairs1),
    inode(
      Mode,
      INodeHash,
      IHierHash,
      SharedPs,
      InHigher,
      NumberOfIdPairs,
      NumberOfPairs2
    )
  ).

