:- module(
  iotw_iimb,
  [
    iimb_experiment/2 % ?N:between(1,80)
                      % -Svg:list(compound)
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(lambda)).
:- use_module(library(oaei/oaei_file)).
:- use_module(library(os/archive_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(set/equiv)).

:- use_module(iotw_experiment).

:- rdf_register_prefix(
     'IIMB',
     'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
   ).





%! iimb_experiment(+N:between(0,80), -Svg:list(compound)) is det.
%! iimb_experiment(-N:between(0,80), -Svg:list(compound)) is multi.
% Calculates the identity hierarchy for every IIMB example (80 items).

iimb_experiment(N, Svg):-
  current_prolog_flag(argv, [Dir|_]),
  absolute_file_name('IIMB.tar.gz', File, [access(read),relative_to(Dir)]),
  
  call_archive_entry(File, 'onto.owl', \Read^rdf_load_file(Read, [graph(base)])),
  
  between(1, 80, N),
  format_integer(N, 3, NNN),

  atomic_concat(NNN, '/onto.owl', OntoEntry),
  atomic_concat(onto_, NNN, G),
  call_archive_entry(File, OntoEntry, \Read^rdf_load_file(Read, [graph(G)])),
  
  atomic_concat(NNN, '/refalign.rdf', RefEntry),
  call_archive_entry(File, RefEntry, \Read^oaei_load_rdf(Read, RefAs0)),

  exclude(is_reflexive_pair, RefAs0, RefAs),
  equiv_pairs_to_sets(RefAs, RefASets),

  iotw_experiment(G, RefASets, Svg, [evaluate(true),granularity(p)]),
  
  file_name_extension(NNN, svg, Out),
  access_file(Out, write),
  xml_write(Svg, Out, [dtd(svg)]).
