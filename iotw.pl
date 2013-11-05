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

Recommendation sharing non-monotonic?

@author Wouter Beek
@version 2013/05, 2013/08-2013/10
*/

:- use_module(generics(ordset_ext)).
:- use_module(iotw(inode)).
:- use_module(iotw(inode_export)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_mat)).
:- use_module(rdf(rdf_serial)).
:- use_module(xsd(xsd)).

:- debug(iotw).



preload_rdfs_voc(O, G2):-
  option(deduction(rdfs), O, none), !,
  absolute_file_name(rdfs(rdfs), File, [access(read),file_type(rdf)]),
  rdf_new_graph(rdfs_voc, G1),
  rdf_load2(File, [graph(G1)]),
  materialize(G1, rdfs),
  rdf_graph_merge([G1], G2).
preload_rdfs_voc(_O, _G).

%! run_experiment(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   +IdentityPairs:list(pair(iri)),
%!   -SVG:list,
%!   -PDF_File:atom
%! ) is det.
% Runs an IOTW experiment.
%
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
  debug(iotw, 'There are ~w identity sets.', [NumberOfISets]),
  
  % DEB: Print the number of identity pairs.
  equivalence_sets_to_number_of_equivalence_pairs(ISets, NumberOfIPairs),
  debug(iotw, 'There are ~w identity pairs.', [NumberOfIPairs]),
  
  % DEB: Print the number of resources.
  aggregate_all(
    sum(CardinalityOfISet),
    (
      member(ISet, ISets),
      length(ISet, CardinalityOfISet)
    ),
    NumberOfResources
  ),
  debug(iotw, 'There are ~w resources.', [NumberOfResources]),

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

  % Make sure that all lexical values that occur in typed literals
  % are canonical values.
  % This makes it much cheaper to establish the identity of typed literals.
  xsd_canonize_graph(G),

  % Materializing the graph reveals additional properties of existing
  % resources, and therefore may reveal additional shared properties.
  option(deduction(Regime), O, none),
  materialize(G, [Regime]),

  % Returns the RDF graph and alignment pairs hash.
  assert_inodes(O, G, ISets, GA_Hash),

  % Create an SVG representation for the given hash.
  export_inodes(O, GA_Hash, SVG, PDF_File).

