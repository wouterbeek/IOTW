:- module(
  iotw_pairs,
  [
    rdf_shared_pairs/2 % +Graph:atom
                       % -Tuples:list(compound)
  ]
).

/** <module> IOTW_PAIRS

Classifies *all* resource pairs by the predicates they share.

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_name)).

:- dynamic(current_assoc(_Assoc)).



%! rdf_shared_pairs(+Graph:atom, -Tuples:list) is det.
% Loads the pairs that share the same properties,
% irrespective of any alignments.
%
% ### Example
%
% Given the following 4 triples:
% ==
% <S1, P, O12>
% <S2, P, O12>
% <S3, P, O34>
% <S4, P, O34>
% ==
%
% The triples would be:
% ==
% <{P},{<S1,S2>,<S3,S4>}>
% ==
%
% @param Stash A list containing tuples of the following form:
%        =|Predicates-NumberOfPredicates-NumberOfPairs-Pairs|=

rdf_shared_pairs(Graph, Stash):-
  rdf_predicates(Graph, Predicates),
  % Collect all length-one predicate sets with
  % their associated resource pairs.
  findall(
    [Predicate]-1-Size-SingletonPairs,
    (
      member(Predicate, Predicates),
      findall(
        Subject1-Subject2,
        (
          % We assume a fully materialized graph.
          rdf(Subject1, Predicate, Object, Graph),
          rdf(Subject2, Predicate, Object, Graph),
          Subject1 @< Subject2
        ),
        SingletonPairs
      ),
      cardinality(SingletonPairs, Size),
      % Sometimes predicates have no extension. Exclude these early on.
      Size > 0
    ),
    SingletonTuples
  ),
  rdf_shared_pairs(SingletonTuples, SingletonTuples, [], Stash).

rdf_shared_pairs([], SingletonTuples, TempStash, SolStash):- !,
  append(SingletonTuples, TempStash, SolStash).
rdf_shared_pairs(
  [PSet-NumberOfPs-_NumberOfPairs-Pairs|Tuples],
  SingletonTuples,
  TempStash,
  SolStash
):-
  findall(
    NewEntry,
    (
      % Find a single predicate that we can add to
      % an existing set of predicates.
      member(SingletonPSet-1-_-SingletonPairs, SingletonTuples),
      \+ ord_subset(SingletonPSet, PSet),
      ord_union(PSet, SingletonPSet, NewPSet),
      % Since a bigger set can be obtained as a result of the union of
      % multiple smaller sets, we make sure that we do not include any
      % duplicates.
      \+ member(NewPSet-_-_-_, TempStash),

      % If the intersection is not empty, then we have found
      % a new set of predicates.
      ord_intersection(Pairs, SingletonPairs, NewPairs),
      \+ ord_empty(NewPairs),

      % The extended set of predicates may be further extensible.
      NewNumberOfPs is NumberOfPs + 1,
      cardinality(NewPairs, NewNumberOfPairs),
      NewEntry = NewPSet-NewNumberOfPs-NewNumberOfPairs-NewPairs
    ),
    NewEntries
  ),

  append(Tuples, NewEntries, NewTuples),
  append(TempStash, NewEntries, NewTempStash),
  rdf_shared_pairs(NewTuples, SingletonTuples, NewTempStash, SolStash).

