:- module(
  iotw,
  [
    run_experiment/4 % +Options:list(nvpair),
                     % +Graph:atom,
                     % +IdentityPairs:list(pair(iri))
                     % -SVG:list
  ]
).

/** <module> IOTW

IOTW experiments.

Recommendation sharing non-monotonic?

@author Wouter Beek
@version 2013/05, 2013/08-2013/11
*/

:- use_module(generics(ordset_ext)).
:- use_module(iotw(inode)).
:- use_module(iotw(inode_export)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdfs(rdfs_voc)).
:- use_module(xsd(xsd)).

:- debug(iotw).



%! run_experiment(
%!   +Options:list(nvpair),
%!   +IdentityPairs:list(pair(iri)),
%!   -SVG_DOM:list,
%!   +Graph:atom
%! ) is det.
% Runs an IOTW experiment.
%
% The following options are supported:
%   * =|granularity(+LevelOfIdentityPartition:oneof([p,po]))|=
%     Whether the identity hierarchy is asserted on the level of
%     shared predicates, or on the level of shared predicate-object pairs.

run_experiment(O1, IPairs, SVG, G):-
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
  aggregate_all(
    count,
    (
      member(ISet, ISets),
      \+ length(ISet, 2),
      debug(iotw, '\tNon-pair identity set: ~w', [ISet])
    ),
    NumberOfNonpairISets
  ),
  debug(iotw, 'Number of non-pair identity sets: ~w', [NumberOfNonpairISets]),

  % Make sure that all lexical values that occur in typed literals
  % are canonical values.
  % This makes it much cheaper to establish the identity of typed literals.
  xsd_canonize_graph(G),

  % Returns the RDF graph and alignment pairs hash.
  assert_inodes(O1, G, ISets, GA_Hash),

  % Create an SVG representation for the given hash.
  export_inodes(O1, GA_Hash, SVG).

