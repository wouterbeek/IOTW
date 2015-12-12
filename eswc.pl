:- module(
  eswc,
  [
    align_pair/2, % ?Number:between(1,80)
                  % -Pair:pair
    calc_fca/1, % ?Number:between(1,80)
    load_base/0,
    load_onto/1, % ?Number:between(1,80)
    print_align/1 % +Pair:pair
  ]
).

/** <module> ESWC

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(fca/fca_viz)).
:- use_module(library(http/http_server)).
:- use_module(library(lists)).
:- use_module(library(oaei/oaei_file)).
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





align_pair(N, Pair):-
  align0(N, Pairs),
  member(Pair, Pairs).

align_rel(N, Rel):-
  align0(N, Pairs),
  equiv(Pairs, Rel).

align0(N, Pairs):-
  data_file(File),
  data_number(N, NNN),
  atomic_concat(NNN, '/refalign.rdf', RefEntry),
  oaei_load_rdf(File, Pairs, [archive_entry(RefEntry)]).



calc_fca(N):-
  load_base,
  load_onto(N),
  align_rel(N, Rel),
  fca_viz(
    context(relation:relation_pair(Rel),rdf_term:rdf_predicate,eswc:rdf_shared),
    File,
    [
      attribute_label(rdf_print_term:rdf_print_term),
      concept_label(both),
      graph_label("ESWC Experiment"),
      object_label(rdf_print_term:rdf_print_term)
    ]
  ),
  open_pdf(File).



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



print_align(X-Y):-
  rdf_compare(X, Y).





% HELPERS %

data_file(File):-
  current_prolog_flag(argv, [Dir|_]),
  absolute_file_name('IIMB.tar.gz', File, [access(read),relative_to(Dir)]).


data_number(N, NNN):-
  between(1, 80, N),
  format_integer(N, 3, NNN).


rdf_shared(X-Y, P):- maplist(nonvar, [X,Y,P]), !, rdf_shared(X, Y, P), !.
rdf_shared(X-Y, P):- rdf_shared(X, Y, P).
rdf_shared(X, Y, P):- rdf(X, P, Z), rdf(Y, P, Z).
