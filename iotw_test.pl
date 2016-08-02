:- module(
  iotw_test,
  [
    test0/0
  ]
).

/** <module> IOTW test module

Simple tests for the IOTW codebase.

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).

:- use_module(iotw(iotw)).



test0:-
  M = rdf
  rdf_equal(ex:iotw_test, G),
  rdf_global_id(ex:'Amsterdam', Amsterdam),
  rdf_global_id(ex:'Andrea', Andrea),
  rdf_global_id(ex:'Berlin', Berlin),
  rdf_global_id(ex:'Boetje', Boetje),
  rdf_global_id(ex:'Wouter', Wouter),

  qb_individual(M, Andrea, ex:'Person', G),
  qb_label(M, Andrea, 'Andrea', G),
  qb_individual(M, Wouter, ex:'Person', G),
  qb_label(M, Wouter, 'Wouter', G),
  qb_individual(M, Boetje, ex:'Person', G),
  qb_label(M, Boetje, 'Boetje', G),

  qb_individual(M, Amsterdam, ex:'Capital', G),
  qb_individual(M, Amsterdam, ex:'City', G),
  qb_individual(M, Amsterdam, ex:'GeoLocation', G),
  qb_label(M, Amsterdam, 'Amsterdam', G),
  qb_individual(M, Berlin, ex:'Capital', G),
  qb_individual(M, Berlin, ex:'City', G),
  qb_individual(M, Berlin, ex:'GeoLocation', G),
  qb_label(M, Berlin, 'Berlin', G),
  
  qb_subproperty(M, rdf:type, ex:typo, G),

  run_experiment(
    G,
    [Andrea-Wouter,Andrea-Boetje,Amsterdam-Berlin],
    _,
    [evaluate(true),granularity(p)]
  ).
