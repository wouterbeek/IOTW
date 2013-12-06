:- module(
  iotw_test,
  [
    test0/0
  ]
).

/** <module> IOTW test module

Simple tests for the IOTW codebase.

@author Wouter Beek
@version 2013/09, 2013/11-2013/12
*/

:- use_module(iotw(iotw)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_lit_build)).



test0:-
  G = iotw_test,
  rdf_global_id(rdf:'Amsterdam', Amsterdam),
  rdf_global_id(rdf:'Andrea', Andrea),
  rdf_global_id(rdf:'Berlin', Berlin),
  rdf_global_id(rdf:'Boetje', Boetje),
  rdf_global_id(rdf:'Wouter', Wouter),

  rdf_assert(Andrea, rdf:type, rdf:'Person', G),
  rdf_assert_literal(Andrea, rdfs:label, 'Andrea', G),
  rdf_assert(Wouter, rdf:type, rdf:'Person', G),
  rdf_assert_literal(Wouter, rdfs:label, 'Wouter', G),
  rdf_assert(Boetje, rdf:type, rdf:'Person', G),
  rdf_assert_literal(Boetje, rdfs:label, 'Boetje', G),

  rdf_assert(Amsterdam, rdf:type, rdf:'Capital', G),
  rdf_assert(Amsterdam, rdf:type, rdf:'City', G),
  rdf_assert(Amsterdam, rdf:type, rdf:'GeoLocation', G),
  rdf_assert_literal(Amsterdam, rdfs:label, 'Amsterdam', G),
  rdf_assert(Berlin, rdf:type, rdf:'Capital', G),
  rdf_assert(Berlin, rdf:type, rdf:'City', G),
  rdf_assert(Berlin, rdf:type, rdf:'GeoLocation', G),
  rdf_assert_literal(Berlin, rdfs:label, 'Berlin', G),
  
  rdf_assert(rdf:type, rdfs:subPropertyOf, rdf:typo, G),

  run_experiment(
    [granularity(p)],
    [Andrea-Wouter,Andrea-Boetje,Amsterdam-Berlin],
    _SVG,
    G
  ).

