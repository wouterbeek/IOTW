:- module(
  iotw_generics,
  [
    number_of_equivalence_pairs/2 % +EquivalenceSets:list(ordset)
                                  % -NumberOfPairs:nonneg
  ]
).

:- use_module(library(aggregate)).
:- use_module(library(lists)).





%! number_of_equivalence_pairs(
%!   +EquivalenceSets:list(ordset),
%!   -NumberOfPairs:nonneg
%! ) is det.
% Returns the number of equivalence pairs that are encoded in
% the given collection of equivalence sets.
%
% Ths excludes the reflexive identity pairs.

number_of_equivalence_pairs(EqSets, NumberOfPairs):-
  aggregate_all(
    sum(NumberOfPairs),
    (
      member(EqSet, EqSets),
      length(EqSet, Cardinality),
      cardinality_to_number_of_pairs(Cardinality, NumberOfPairs)
    ),
    NumberOfPairs
  ).

cardinality_to_number_of_pairs(Cardinality, NumberOfPairs):-
  NumberOfSymmetricAndTransitivePairs is Cardinality * (Cardinality - 1),
  NumberOfPairs = NumberOfSymmetricAndTransitivePairs.
  % To include reflexive identity pairs:
  %NumberOfPairs is NumberOfSymmetricAndTransitivePairs + Cardinality.
