:- module(
  iotw,
  [
    run_experiment/6 % +Options:list(nvpair),
                     % +Graph:atom,
                     % +NumberOfIdentityPairs:nonneg,
                     % +IdentitySets:list(ordset(iri))
                     % -SVG:list
                     % -PDF_File:atom
  ]
).

/** <module> IOTW

IOTW experiments.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09
*/

:- use_module(iotw(inode)).
:- use_module(iotw(inode_export)).
:- use_module(library(option)).
:- use_module(logic(rdf_axiom)).
:- use_module(rdf(rdf_serial)).



preload_rdfs_voc(O, G):-
  option(deduction(rdfs), O, none), !,
  absolute_file_name(rdfs(rdfs), File, [access(read),file_type(rdf)]),
  rdf_load2(File, [graph(G)]),
  materialize(G).
preload_rdfs_voc(_O, _G).

%! run_experiment(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +NumberOfIdentityPairs:nonneg,
%!   +IdentitySets:list(ordset(iri)),
%!   -SVG:list,
%!   -PDF_File:atom
%! ) is det.
% The following options are supported:
%   * `deduction(+DeductionMode:oneof([none,rdfs])`
%     The default is `none`.
%   * `granularity(+LevelOfIdentityPartition:oneof([p,po]))`
%     Whether the identity hierarchy is asserted on the level of
%     shared predicates, or on the level of shared predicate-object pairs.

run_experiment(O, G, NumberOfIdPairs, ISets, SVG, PDF_File):-
  % Pre-load the RDF(S) vocabulary.
  % This means that materialization has to make less deductions
  % (tested on 163 less), and there are some labels and comments
  % that deduction would not produce.
  preload_rdfs_voc(O, G),

  % Materializing the graph reveals additional properties of existing
  % resources, and therefore may reveal additional shared properties.
  (
    option(deduction(none), O, none), !
  ;
    materialize(G)
  ),

  % Returns the RDF graph and alignment pairs hash.
  assert_identity_nodes(O, G, NumberOfIdPairs, ISets, GA_Hash),

  % Create an SVG representation for the given hash.
  export_identity_nodes(GA_Hash, SVG, PDF_File).

