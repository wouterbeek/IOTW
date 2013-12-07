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
@version 2013/05, 2013/08-2013/12
*/

:- use_module(generics(ordset_ext)).
:- use_module(iotw(inode)).
:- use_module(iotw(inode_export)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(xsd(xsd)).

:- dynamic(result/2).

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
%
% @param Options A list of name-value pairs.
% @param IdentityPairs A list of alignment pairs,
%        thus excluding (possibly) the reflexive cases.
% @param SVG The DOM of an ihierarchy.
% @param Graph The atomic name of an RDF graph.

run_experiment(O1, IPairs1, SVG, G):-
  % Make sure there are no reflexive pairs.
  % In the absence of another pair this would result in
  % a singleton identity sets.
  % This could result in many inodes that are
  % particular to a single resource
  % (since something shares all its properties with itself).
  exclude(is_reflexive_pair, IPairs1, IPairs2),

  % Clear data store.
  inode:clear_db,

  % Retrieve all alignment sets.
  % Does not include singleton sets (due to reflexivity).
  pairs_to_ord_sets(IPairs2, ISets),

  (
    debugging(iotw, false), !
  ;
    % DEB: Print the number of identity sets.
    length(ISets, NumberOfISets),
    debug(iotw, 'There are ~d identity sets.', [NumberOfISets]),

    % DEB: Print the number of identity pairs.
    %      Note that not all identity pairs may have been explicit
    %      in the original collection of pairs.
    equivalence_sets_to_number_of_equivalence_pairs(ISets, NumberOfIPairs),
    debug(iotw, 'There are ~d identity pairs.', [NumberOfIPairs]),

    % DEB: Print the number of resources.
    aggregate_all(
      sum(CardinalityOfISet),
      (
        member(ISet, ISets),
        length(ISet, CardinalityOfISet)
      ),
      NumberOfResources
    ),
    debug(iotw, 'There are ~d resources.', [NumberOfResources]),

    % DEB: Print the number of non-pair identity sets.
    %      This quantifies the usefulness of using sets instead of pairs.
    aggregate_all(
      count,
      (
        member(ISet, ISets),
        \+ length(ISet, 2)
      ),
      NumberOfNonpairISets
    ),
    debug(
      iotw,
      'Number of non-pair identity sets: ~d',
      [NumberOfNonpairISets]
    )
  ),

  % Make sure that all lexical values that occur in typed literals
  % are canonical values.
  % This makes it much cheaper to establish the identity of typed literals.
  xsd_canonize_graph(G),

  % Returns the RDF graph and alignment pairs hash.
  assert_inodes(O1, G, ISets, GA_Hash),

  % Create an SVG representation for the given hash.
  export_inodes(O1, GA_Hash, SVG),

  % Run the evaluation.
  %%evaluate_inodes(O1, GA_Hash, Recalls),
  %%assert(result(GA_Hash, Recalls)),

  true.

%! equivalence_sets_to_number_of_equivalence_pairs(
%!   +EquivalenceSets:list(ordset),
%!   +NumberOfEquivalencePairs:nonneg
%! ) is det.
% Returns the number of equivalence pairs that are encoded in
% the given collection of equivalence sets.
%
% We do not count reflexive cases.
% We do count symmetric cases.
%
% @tbd Should this predicate really be here?

equivalence_sets_to_number_of_equivalence_pairs(EqSets, NumberOfEqPairs):-
  aggregate_all(
    sum(NumberOfEqPairs__),
    (
      member(EqSet, EqSets),
      length(EqSet, NumberOfMembers),
      % No reflexive cases.
      NumberOfEqPairs__ is NumberOfMembers * (NumberOfMembers - 1)
    ),
    NumberOfEqPairs
  ).

is_reflexive_pair(X-X).
