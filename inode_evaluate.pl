:- module(
  inodes_evaluate,
  [
    evaluate_inodes/2 % +IdentityHierarchyHash:atom
                      % +Options:list(compound)
  ]
).

/** <module> Identity nodes evaluation

Evaluates results from identity experiments.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(debug)).
:- use_module(library(lod/lod_stats)).
:- use_module(library(math/math_ext)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(random)).
:- use_module(library(set/equiv)).

:- use_module(inode).

:- predicate_options(evaluate_inodes/2, 2, [
     pass_to(evaluate_inodes/5, 5)
   ]).
:- predicate_options(evaluate_inodes/5, 5, [
     pass_to(evaluate_inodes/4, 4)
   ]).
:- predicate_options(evaluate_inodes/4, 4, [
     pass_to(create_ihier/4, 4)
   ]).





%! evaluate_inodes(+IHierHash:atom, +Options:list(compound)) is det.
% Evaluates the given identity hierarcy,

evaluate_inodes(IHierHash, Opts):-
  absolute_file_name("stats.csv", File, [access(write)]),
  setup_call_cleanup(
    open(File, append, Write),
    evaluate_inodes(1.0, 0.05, IHierHash, Write, Opts),
    close(Write)
  ).


%! evaluate_inodes(
%!   +Perc:between(0.0,1.0),
%!   +DeltaPerc:between(0.0,1.0),
%!   +IHierHash:atom,
%!   +Write:stream,
%!   +Options:list(compound)
%! ) is det.
% Evaluates the given identity hierarchy for a sequence of percentages.

evaluate_inodes(Perc, DeltaPerc, _, _, _):-
  Perc < DeltaPerc, !.
evaluate_inodes(Perc1, DeltaPerc, IHierHash, Write, Opts):-
  evaluate_inodes(Perc1, IHierHash, Write, Opts),
  Perc2 is Perc1 - DeltaPerc,
  evaluate_inodes(Perc2, DeltaPerc, IHierHash, Write, Opts).


%! evaluate_inodes(
%!   +Perc:between(0.0,1.0),
%!   +IHierHash:atom,
%!   +Write:string,
%!   +Options:list(compound)
%! ) is det.
% Evaluates the given identity hierarchy for a specific percentage.

evaluate_inodes(Perc, GA_Hash1, Write, Opts):-
  % Create the reduced identity hierarchy.
  once(ihier(G, GA_Hash1, ISets1, _P_Assoc1, _, _)),
  random_subset_perc(ISets1, 0.1, ISets2, ISets3),

  create_ihier(G, ISets2, GA_Hash2, Opts),

  % Higher approximation recall.
  assoc_to_higher_pairs(GA_Hash1, H_Approx1),
  assoc_to_higher_pairs(GA_Hash2, H_Approx2),
  ord_intersection(H_Approx1, H_Approx2, H_Approx12),
  maplist(length, [H_Approx1,H_Approx2,H_Approx12], [H1,H2,H12]),
  float_div_zero(H12, H1, HRecall),

  % Lower approximation recall.
  assoc_to_lower_pairs(GA_Hash1, L_Approx1),
  assoc_to_lower_pairs(GA_Hash2, L_Approx2),
  ord_intersection(L_Approx1, L_Approx2, L_Approx12),
  maplist(length, [L_Approx1,L_Approx2,L_Approx12], [L1,L2,L12]),
  float_div_zero(L12, L1, LRecall),

  % Quality
  maplist(float_div_zero, [L1,L2], [H1,H2], [Q1,Q2]),

  % Higher cover.
  rdf_number_of_subjects(_, _, G, NumberOfSubjectTerms),
  % No reflexive cases.
  NumberOfPairs is NumberOfSubjectTerms * (NumberOfSubjectTerms - 1),
  float_div_zero(H2, NumberOfPairs, HCover),

  % Can the extracted alignments be found?
  equiv_partition(IPairs3, ISets3),
  ord_intersection(IPairs3, H_Approx2, H_IPairs3),
  maplist(length, [IPairs3,H_IPairs3], [IPairs3_Length,H_IPairs3_Length]),
  float_div_zero(H_IPairs3_Length, IPairs3_Length, H_IPairs3_Perc),
  
  % DEB
  RemPerc is 1.0 - Perc,
  debug(inodes_evaluate, '~6f of alignments removed.', [RemPerc]),
  debug(inodes_evaluate, '~6f of removed alignments are in higher.', [H_IPairs3_Perc]),
  debug(inodes_evaluate, '~6f of all pairs are in higher.', [HCover]),
  H_IPairs3_Perc_Corrected is H_IPairs3_Perc - HCover,
  debug(inodes_evaluate, '~6f of all pairs are in higher (corrected).', [H_IPairs3_Perc_Corrected]),
  debug(inodes_evaluate, 'Recall::\tLow:~6f\tHigh:~6f', [LRecall,HRecall]),
  debug(inodes_evaluate, 'Quality::\t~6f->~6f', [Q1,Q2]),

  csv_write_stream(
    Write,
    [row(Perc,LRecall,HRecall,Q2,HCover,H_IPairs3_Perc,H_IPairs3_Perc_Corrected)],
    []
  ),
  flush_output(Write).

assoc_to_higher_pairs(IHierHash, Pairs):-
  assoc_to_pairs(IHierHash, higher, Pairs).

assoc_to_lower_pairs(IHierHash, Pairs):-
  assoc_to_pairs(IHierHash, lower, Pairs).

assoc_to_pairs(IHierHash, Approx1, Pairs2):-
  findall(
    Pairs1,
    (
      approx(Approx1, Approx2),
      inode(_, _, IHierHash, _, Approx2, _, _, _, Pairs1)
    ),
    Pairss
  ),
  ord_union(Pairss, Pairs2).

approx(X, X).
approx(higher, lower).





% HELPERS %

%! random_subset(
%!   +Set:ordset,
%!   +Length:nonneg,
%!   -Subset:ordset,
%!   -Rest:ordset
%! ) is det.

random_subset(X, M, Y0, Z0):-
  length(X, N),
  (M =< N // 2 -> Y = Y0, Z = Z0 ; Y = Z0, Z = Y0),
  random_subset(X, M, M, Y, Z).

random_subset(Z, M, M, [], Z):- !.
random_subset(X1, M1, N, [H|Y], Z):-
  random_select(H, X1, X2),
  succ(M2, M1),
  random_subset(X2, M2, N, Y, Z).



%! random_subset_perc(
%!   +Set:ordset,
%!   +Percentage:between(0.0,1.0),
%!   -Subset:ordset,
%!   -Rest:ordset
%! ) is det.

random_subset_perc(X, Perc, Y, Z):-
  length(X, N),
  M is floor(N * Perc),
  random_subset(X, M, Y, Z).
