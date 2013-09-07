:- module(
  iotw_test,
  [
    test0/0
  ]
).

/** <module> IOTW test module

Simple tests for the IOTW codebase.

@author Wouter Beek
@version 2013/09
*/

:- use_module(iotw(iotw)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(run_ext)).



test0:-
  G = iotw_test,
  rdf_global_id(rdf:'Amsterdam', Amsterdam),
  rdf_global_id(rdf:'Andrea', Andrea),
  rdf_global_id(rdf:'Berlin', Berlin),
  rdf_global_id(rdf:'Boetje', Boetje),
  rdf_global_id(rdf:'Wouter', Wouter),

  rdf_assert(Andrea, rdf:type, rdf:'Person', G),
  rdf_assert(Wouter, rdf:type, rdf:'Person', G),
  rdf_assert(Boetje, rdf:type, rdf:'Person', G),

  rdf_assert(Amsterdam, rdf:type, rdf:'Capital', G),
  rdf_assert(Amsterdam, rdf:type, rdf:'City', G),
  rdf_assert(Amsterdam, rdf:type, rdf:'GeoLocation', G),
  rdf_assert(Berlin, rdf:type, rdf:'Capital', G),
  rdf_assert(Berlin, rdf:type, rdf:'City', G),
  rdf_assert(Berlin, rdf:type, rdf:'GeoLocation', G),
  
  rdf_assert(rdf:type, rdfs:subPropertyOf, rdf:typo, G),
  
  list_to_ord_set([Andrea,Wouter,Boetje], ISet1),
  list_to_ord_set([Amsterdam,Berlin], ISet2),
  run_experiment(
    [deduction(rdfs),granularity(po)],
    G,
    [ISet1,ISet2],
    _SVG,
    PDF_File
  ),
  open_pdf(PDF_File).

