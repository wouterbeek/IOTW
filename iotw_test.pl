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

:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdfs/rdfs_build)).

:- use_module(iotw(iotw)).



test0:-
  G = iotw_test,
  rdf_global_id(ex:'Amsterdam', Amsterdam),
  rdf_global_id(ex:'Andrea', Andrea),
  rdf_global_id(ex:'Berlin', Berlin),
  rdf_global_id(ex:'Boetje', Boetje),
  rdf_global_id(ex:'Wouter', Wouter),

  rdf_assert_individual(Andrea, ex:'Person', G),
  rdfs_assert_label(Andrea, 'Andrea', G),
  rdf_assert_individual(Wouter, ex:'Person', G),
  rdfs_assert_label(Wouter, 'Wouter', G),
  rdf_assert_individual(Boetje, ex:'Person', G),
  rdfs_assert_label(Boetje, 'Boetje', G),

  rdf_assert_individual(Amsterdam, ex:'Capital', G),
  rdf_assert_individual(Amsterdam, ex:'City', G),
  rdf_assert_individual(Amsterdam, ex:'GeoLocation', G),
  rdfs_assert_label(Amsterdam, 'Amsterdam', G),
  rdf_assert_individual(Berlin, ex:'Capital', G),
  rdf_assert_individual(Berlin, ex:'City', G),
  rdf_assert_individual(Berlin, ex:'GeoLocation', G),
  rdfs_assert_label(Berlin, 'Berlin', G),
  
  rdfs_assert_subproperty(rdf:type, ex:typo, G),

  run_experiment(
    G,
    [Andrea-Wouter,Andrea-Boetje,Amsterdam-Berlin],
    _,
    [evaluate(true),granularity(p)]
  ).
