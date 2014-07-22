:- module(
  iotw,
  [
    run_experiment/4 % +Graph:atom
                     % +IdentitySets:ordset(ordset(iri))
                     % -Svg:dom
                     % +Options:list(nvpair)
  ]
).

/** <module> IOTW

IOTW experiments.

@author Wouter Beek
@version 2013/05, 2013/08-2013/12, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(predicate_options)). % Declarations.

:- use_module(generics(deb_ext)).
:- use_module(generics(pair_ext)).
:- use_module(logic(equiv)).
:- use_module(xsd(xsd_clean)).

:- use_module(iotw(inode)).
:- use_module(iotw(inode_evaluate)).
:- use_module(iotw(inode_export)).

:- predicate_options(run_experiment/4, 4, [
     pass_to(create_ihier/4, 4),
     pass_to(export_ihier_as_svg/3, 3),
     evaluate(+boolean)
   ]).



%! run_experiment(
%!   +Graph:atom,
%!   +IdentitySets:ordset(ordset(iri)),
%!   -Svg:dom,
%!   +Options:list(nvpair)
%! ) is det.
% Runs an IOTW experiment.
%
% ### Arguments
%
% @arg Options A list of name-value pairs.
% @arg IdentitySets ...
% @arg SVG The DOM of an ihierarchy.
% @arg Graph The atomic name of an RDF graph.
%
% ### Options
%
% The following options are supported:
%   * =|evaluate(+RunEvaluation:boolean)|=
%     Whether the evaluation is run on the data or not.
%     See module IOTW_EVALUATE.
%   * =|granularity(+LevelOfIdentityPartition:oneof([p,po]))|=
%     Whether the identity hierarchy is asserted on the level of
%     shared predicates, or on the level of shared predicate-object pairs.

run_experiment(Graph, ISets, Svg, Options):-
/* @tbd document
  % Make sure there are no reflexive pairs.
  % A reflexive pair or singleton iset would probably result in
  % an inode that is particular to a single resource,
  % since something shares all of its properties with itself.
  exclude(is_reflexive_pair, IPairs1, IPairs2),

  % Retrieve all alignment sets.
  % Notice that there are no singleton sets,
  % since the identity pairs have been filtered for reflexivity.
  pairs_to_sets(IPairs2, ISets),
*/

  % DEB
  if_debug(iotw, begin_experiment(ISets, NumberOfIPairs)),

  % Make sure that all lexical values that occur in typed literals
  % are canonical values.
  % This makes establishing the identity of typed literals more efficient.
  xsd_canonize_graph(Graph),

  % Returns the RDF graph and alignment pairs hash.
  create_ihier(Graph, ISets, IHierHash, Options),

  % Create an SVG representation for the given hash.
  export_ihier_as_svg(IHierHash, Svg, Options),

  % DEB
  if_debug(iotw, end_experiment(IHierHash, NumberOfIPairs)),

  % Run the evaluation.
  (
    option(evaluate(false), Options, false), !
  ;
    evaluate_inodes(IHierHash, Options)
  ),

  % Done!
  clear_ihiers.

begin_experiment(ISets, NumberOfIPairs):-
  % Print the number of identity sets.
  length(ISets, NumberOfISets),
  debug(iotw, 'There are ~:d identity sets.', [NumberOfISets]),

  % Print the number of identity pairs.
  % Note that not all identity pairs may have been explicit
  % in the original collection of pairs.
  number_of_equivalence_pairs(ISets, NumberOfIPairs),
  debug(iotw, 'There are ~:d identity pairs.', [NumberOfIPairs]),

  % Print the number of resources.
  aggregate_all(
    sum(CardinalityOfISet),
    (
      member(ISet, ISets),
      length(ISet, CardinalityOfISet)
    ),
    NumberOfResources
  ),
  debug(iotw, 'There are ~:d resources.', [NumberOfResources]),

  % Print the number of non-pair identity sets.
  % This quantifies the usefulness of using sets instead of pairs.
  aggregate_all(
    count,
    (
      member(ISet, ISets),
      \+ length(ISet, 2)
    ),
    NumberOfNonpairISets
  ),
  debug(iotw, 'Number of non-pair identity sets: ~:d', [NumberOfNonpairISets]).

end_experiment(IHierHash, NumberOfAllIPairs1):-
  aggregate_all(
    sum(NumberOfIPairs),
    inode(_, _, IHierHash, _, _, NumberOfIPairs, _, _, _),
    NumberOfAllIPairs2
  ),
  (
    NumberOfAllIPairs1 =:= NumberOfAllIPairs1
  ->
    true
    %debug(iotw, 'The number of ipairs matches.', [])
  ;
    debug(
      iotw,
      'Number of ipairs does not match: ~:d and ~:d.',
      [NumberOfAllIPairs1,NumberOfAllIPairs2]
    )
  ).

is_reflexive_pair(X-X).

