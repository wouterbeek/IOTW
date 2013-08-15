:- module(
  iotw_alignments,
  [
    alignments_by_predicates/3 % +Graph:atom
                               % +Alignments:list(pair)
                               % -Predicates:assoc
  ]
).

/** <module> IOTW_ALIGNMENTS

Classifies alignment resource pairs by the predicates they share.

Input: a list of alginment pairs.

Output: an association list with predicate sets as keys and
        subsets of the alignment pairs as values.

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(iotw(iotw)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).



%! alignments_by_predicates(
%!   +Graph:atom,
%!   +Alignments:list(pair),
%!   -Predicates:assoc
%! ) is det.
% Returns an association list with predicate keys
% and alignment pair set values.

alignments_by_predicates(Graph, Alignments, Predicates):-
  empty_assoc(EmptyAssoc),
  alignments_by_predicates(Graph, EmptyAssoc, Alignments, Predicates).

%! alignments_by_predicates(
%!   +Graph:atom,
%!   +OldPredicates:assoc,
%!   +Alignments:list(pair),
%!   -NewPredicates:assoc
%! ) is det.

alignments_by_predicates(_Graph, SolAssoc, [], SolAssoc):- !.
alignments_by_predicates(Graph, OldAssoc, [From-To|Alignments], SolAssoc):-
  % Take the predicates that the alignment pair shares.
  rdf_shared_pair(From, To, Predicates),
  % Add the alignment pair as a value to the predicates key.
  put_assoc(Predicates, OldAssoc, From-To, NewAssoc),
  alignments_by_predicates(Graph, NewAssoc, Alignments, SolAssoc).

%! rdf_shared_pair(
%!   +Subject1:uri,
%!   +Subject2:uri,
%!   -Predicates:ordset(uri)
%! ) is det.
% Returns the predicates that both subjects possess.
%
% Moves from pairs to predicates.

rdf_shared_pair(Subject1, Subject2, Solution):-
  rdf_shared_pair(Subject1, Subject2, [], Solution).

rdf_shared_pair(Subject1, Subject2, OldPredicates, Solution):-
  % We assume a fully materialized graph.
  rdf(Subject1, Predicate1, Object1, Graph),
  % We are looking for new predicates only.
  \+ member(Predicate1, OldPredicates),
  same_predicate(Predicate1, Predicate2),
  rdf(Subject2, Predicate2, Object2, Graph),
  % @tbd If the alignments are assured to be ordered in a specific way,
  %      then `@<` could be used here instead (more efficient).
  Subject1 \== Subject2,
  same_object(Object1, Object2),
  !,
  ord_add_element(OldPredicates, Predicate1, NewPredicates),
  rdf_shared_pair(Subject1, Subject2, NewPredicates, Solution).
rdf_shared_pair(_Subject1, _Subject2, Predicates, Predicates).

