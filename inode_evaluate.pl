:- module(
  inodes_evaluate,
  [
    evaluate_inodes/3 % +Options:list(nvpair)
                      % +IdentityHierarchyHash:atom
                      % -Recalls:list(float)
  ]
).

/** <module> Identity nodes evaluation

Evaluates results from identity experiments.

@author Wouter Beek
@version 2013/12
*/

:- use_module(generics(set_theory)).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(math_ext)).
:- use_module(iotw(inode)).
:- use_module(rdf(rdf_stat)).

:- set_prolog_stack(global, limit(2*10**9)).

:- debug(inodes_evaluate).



evaluate_inodes(O1, GA_Hash, Recalls):-
  evaluate_inodes(O1, 1.0, GA_Hash, [], Recalls).

evaluate_inodes(_O1, Perc, _GA_Hash, Sol, Sol):-
  Perc =< 0.01, !.
evaluate_inodes(O1, Perc1, GA_Hash, T, Sol):-
  evaluate_inodes(O1, Perc1, GA_Hash, H),
  Perc2 is Perc1 - 0.05,
  evaluate_inodes(O1, Perc2, GA_Hash, [H|T], Sol).

evaluate_inodes(O1, Perc, GA_Hash1, LRecall-HRecall):-
  % Create the reduced identity hierarchy.
  once(ihier(GA_Hash1, G, ISets1, _P_Assoc1, _, _)),
  random_subset(ISets1, Perc, ISets2),
  assert_inodes(O1, G, ISets2, GA_Hash2),

  % Higher approximation precision.
  assoc_to_higher_pairs(GA_Hash1, H_Approx1),
  assoc_to_higher_pairs(GA_Hash2, H_Approx2),
  ord_intersection(H_Approx1, H_Approx2, H_Approx12),
  maplist(length, [H_Approx1,H_Approx2,H_Approx12], [H1,H2,H12]),
  divide(H12, H2, HPrecision),
  divide(H12, H1, HRecall),

  % Lower approximation precision.
  assoc_to_lower_pairs(GA_Hash1, L_Approx1),
  assoc_to_lower_pairs(GA_Hash2, L_Approx2),
  ord_intersection(L_Approx1, L_Approx2, L_Approx12),
  maplist(length, [L_Approx1,L_Approx2,L_Approx12], [L1,L2,L12]),
  divide(L12, L2, LPrecision),
  divide(L12, L1, LRecall),

  % Lower and higher approximation cover.
  count_subjects(_P, _O, G, NumberOfSubjectTerms),
  NumberOfPairs is NumberOfSubjectTerms ** 2,
  divide(H1, NumberOfPairs, HCover1),
  divide(H2, NumberOfPairs, HCover2),
  divide(L1, NumberOfPairs, LCover1),
  divide(L2, NumberOfPairs, LCover2),

  % Quality
  maplist(divide, [L1,L2], [H1,H2], [Q1,Q2]),

  % DEB
  debug(inodes_evaluate, 'Percentage: ~2f', [Perc]),
  debug(
    inodes_evaluate,
    'Cover::\tLow:~4f->~4f\tHigh:~4f->~4f',
    [LCover1,LCover2,HCover1,HCover2]
  ),
  debug(
    inodes_evaluate,
    'Precision::\tLow:~2f\tHigh:~2f',
    [LPrecision,HPrecision]
  ),
  debug(inodes_evaluate, 'Recall::\tLow:~2f\tHigh:~2f', [LRecall,HRecall]),
  debug(inodes_evaluate, 'Quality::\t~4f->~4f', [Q1,Q2]).

assoc_to_higher_pairs(GA_Hash, Pairs):-
  assoc_to_pairs(GA_Hash, higher, Pairs).

assoc_to_lower_pairs(GA_Hash, Pairs):-
  assoc_to_pairs(GA_Hash, lower, Pairs).

assoc_to_pairs(GA_Hash, Approx1, Pairs2):-
  findall(
    Pairs1,
    (
      approx(Approx1, Approx2),
      inode(_, _, GA_Hash, _, Approx2, _, _, Pairs1)
    ),
    Pairss
  ),
  ord_union(Pairss, Pairs2).

approx(X, X).
approx(higher, lower).

divide(X, Y, 1.0):-
  X =:= Y, !.
divide(X, _, 0.0):-
  X =:= 0.0, !.
divide(_, Y, _):-
  Y =:= 0.0, !,
  gtrace.
divide(X, Y, Z):-
  Z is X / Y.
