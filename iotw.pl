:- module(
  iotw,
  [
    run_experiment/5 % +Options:list(nvpair),
                     % +Graph:atom,
                     % +IdentityPairs:list(pair(iri))
                     % -SVG:list
                     % -PDF_File:atom
  ]
).

/** <module> IOTW

IOTW experiments.

## Changes in the 2013/08-2013/09 version w.r.t. the 2013/05 version

  * Optimization (assoc AVL tree ordsets)
  * Identity sets i.o. identity pairs.
  * RDF(S) materialization
  * PO subpartitions
  * Literal identity via canonical form.
  * Predicate paths (extending predicates).
  * Modular rewrite
  * GV DCG reuse

## TODO

  * JS callback (PO, quality)
  * Multi-agent materialization
  * SKOS, OWL identity reasoning

Recommendation sharing non-monotonic?

@author Wouter Beek
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(ordset_ext)).
:- use_module(iotw(inode)).
:- use_module(iotw(inode_export)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(logic(rdf_axiom)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(xsd(xsd)).

:- debug(iotw).



preload_rdfs_voc(O, G2):-
  option(deduction(rdfs), O, none), !,
  absolute_file_name(rdfs(rdfs), File, [access(read),file_type(rdf)]),
  rdf_new_graph(rdfs_voc, G1),
  rdf_load2(File, [graph(G1)]),
  materialize(G1),
  rdf_graph:rdf_graph_merge([G1], G2).
preload_rdfs_voc(_O, _G).

%! run_experiment(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +IdentityPairs:list(pair(iri)),
%!   -SVG:list,
%!   -PDF_File:atom
%! ) is det.
% The following options are supported:
%   * `deduction(+DeductionMode:oneof([none,rdfs])`
%     The default is `none`.
%   * `granularity(+LevelOfIdentityPartition:oneof([p,po]))`
%     Whether the identity hierarchy is asserted on the level of
%     shared predicates, or on the level of shared predicate-object pairs.

run_experiment(O, G, IPairs, SVG, PDF_File):-
  % Retrieve all alignment sets.
  pairs_to_ord_sets(IPairs, ISets),

  % DEB: Print the number of identity sets.
  length(ISets, NumberOfISets),
  aggregate_all(
    sum(CardinalityOfISet),
    (
      member(ISet, ISets),
      length(ISet, CardinalityOfISet)
    ),
    NumberOfResources
  ),
  debug(
    iotw,
    'There are ~w alignment sets over ~w resources.',
    [NumberOfISets,NumberOfResources]
  ),

  % DEB: Print the number of non-pair identity sets.
  %      This quantifies the usefulness of using sets instead of pairs.
  forall(
    (
      member(ISet, ISets),
      \+ length(ISet, 2),
      flag(number_of_nonpair_identity_sets, Count, Count + 1)
    ),
    debug(iotw, '\tNon-pair identity set: ~w', [ISet])
  ),
  flag(number_of_nonpair_identity_sets, NumberOfNonpairISets, 0),
  debug(iotw, 'Number of non-pair identity sets: ~w', [NumberOfNonpairISets]),

  % Pre-load the RDF(S) vocabulary.
  % This means that materialization has to make less deductions
  % (tested on 163 less), and there are some labels and comments
  % that deduction would not produce.
  preload_rdfs_voc(O, G),

  xsd_canonize_graph(G),

  % Materializing the graph reveals additional properties of existing
  % resources, and therefore may reveal additional shared properties.
  (
    option(deduction(none), O, none), !
  ;
    materialize(G)
  ),

  % Returns the RDF graph and alignment pairs hash.
  assert_inodes(O, G, ISets, GA_Hash),

  % Create an SVG representation for the given hash.
  export_inodes(O, GA_Hash, SVG, PDF_File).

