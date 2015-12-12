:- module(
  eswc,
  [
    calc_fca/1, % ?Number:between(1,80)
    load_base/0,
    load_onto/1, % ?Number:between(1,80)
    print_align/1 % ?Number:between(1,80)
  ]
).

/** <module> ESWC

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(fca/fca_viz)).
:- use_module(library(http/http_server)).
:- use_module(library(list_ext)).
:- use_module(library(math/math_ext)).
:- use_module(library(oaei/oaei_file)).
:- use_module(library(ordsets)).
:- use_module(library(os/external_program)).
:- use_module(library(os/pdf)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_compare)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(set/equiv)).
:- use_module(library(set/relation)).
:- use_module(library(solution_sequences)).
:- use_module(library(tab/tab)).

:- rdf_register_prefix(iimba, 'http://oaei.ontologymatching.org/2012/IIMBDATA/').
:- rdf_register_prefix(iimbt, 'http://oaei.ontologymatching.org/2012/IIMBTBOX/').
:- rdf_register_prefix(iimbx, 'http://www.instancematching.org/IIMB2012/ADDONS#').

:- initialization(init).
init:- list_external_programs, start_server.





calc_fca(N):-
  load_base,
  load_onto(N),
  align_pairs(N, Pairs),
  fca_viz(
    context(eswc:member0(Pairs),rdf_term:rdf_predicate,eswc:rdf_shared),
    File,
    [concept_label(eswc:concept_label(Pairs)),graph_label("ESWC Experiment")]
  ),
  open_pdf(File).
concept_label(Pairs, Concept, Lbl):- string_phrase(concept_label(Pairs, Concept), Lbl).
member0(L, X):- member(X, L).



load_base:-
  data_file(File),
  rdf_expand_ct(ex:base, G),
  rdf_load_file(File, [archive_entry('onto.owl'),graph(G)]).



load_onto(N):-
  data_file(File),
  data_number(N, NNN),
  atomic_concat(NNN, '/onto.owl', OntoEntry),
  rdf_expand_rt(ex:NNN, G),
  rdf_load_file(File, [archive_entry(OntoEntry),graph(G)]).



print_align(N):-
  load_base,
  load_onto(N),
  align_pair(N, X-Y),
  rdf_compare(X, Y).





% HELPERS %

align_pair(N, Pair):-
  align_pairs(N, Pairs),
  member(Pair, Pairs).


align_pairs(N, Pairs):-
  data_file(File),
  data_number(N, NNN),
  atomic_concat(NNN, '/refalign.rdf', RefEntry),
  oaei_load_rdf(File, Pairs, [archive_entry(RefEntry)]).


attribute_label(As) -->
  set(rdf_print_term, As).


concept_label(Pairs, concept(Os,As)) -->
  object_label(Pairs, Os),
  " ",
  attribute_label(As).


data_file(File):-
  current_prolog_flag(argv, [Dir|_]),
  absolute_file_name('IIMB.tar.gz', File, [access(read),relative_to(Dir)]).


data_number(N, NNN):-
  between(1, 80, N),
  format_integer(N, 3, NNN).


object_label(Pairs, Os) -->
  {
    length(Os, N),
    ord_intersection(Pairs, Os, Int),
    length(Int, M)
  },
  pl_term(M),
  "/",
  pl_term(N),
  " ",
  percentage(M, N).


percentage(M, N) -->
  {float_div_zero(M, N, X), Perc is floor(X * 100)},
  pl_term(Perc).


rdf_shared(X-Y, P):- maplist(nonvar, [X,Y,P]), !, rdf_shared(X, Y, P), !.
rdf_shared(X-Y, P):- rdf_shared(X, Y, P).
rdf_shared(X, Y, P):- rdf(X, P, Z), rdf(Y, P, Z).
