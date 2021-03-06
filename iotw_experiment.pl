:- module(
  iotw_experiment,
  [
    iotw_experiment/4 % +Graph:atom
                      % +IdentitySets:ordset(ordset(iri))
                      % -Dom:list(compound)
                      % +Options:list(compound)
  ]
).

/** <module> IOTW Experiments

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(q/qu)).

:- use_module(iotw(inode)).
:- use_module(iotw(inode_evaluate)).
:- use_module(iotw(inode_export)).
:- use_module(iotw(iotw_generics)).





%! iotw_experiment(
%!   +Graph:atom,
%!   +IdentitySets:ordset(ordset(iri)),
%!   -Dom:list(compound),
%!   +Options:list(compound)
%! ) is det.
% Runs an IOTW experiment.
%
% ### Arguments
%
% @arg IdentitySets An ordered set of ordered sets of resources.
%      The resources within the same set are identical to each other.
%      When calling this predicates, it is wise to exclude
%      identity sets that are singleton sets.
%      These correspond to reflexive identity pairs,
%      so filtering for reflexive pairs and then converting pairs to sets
%      also works.
%      The reason for excluding singleton identity sets is that
%      the corresponding inode would contain all the properties
%      of a single resource.
% @arg Dom The DOM of an ihierarchy.
% @arg Options A list of name-value pairs.
%
% ### Options
%
% The following options are supported:
%   * evaluate(+boolean)
%     Whether the evaluation is run on the data or not.
%     See module IOTW_EVALUATE.
%   * granularity(+oneof([p,po]))
%     Whether the identity hierarchy is asserted on the level of
%     shared predicates, or on the level of shared predicate-object pairs.

iotw_experiment(G, ISets, Dom, Opts):-
  if_debug(iotw, begin_experiment(ISets, NumberOfIPairs)),

  % Make sure that all lexical values that occur in typed literals
  % are canonical values.
  % This makes establishing the identity of typed literals more efficient.
  %%%%rdf_canonize_graph(G),

  % Returns the RDF graph and alignment pairs hash.
  create_ihier(G, ISets, IHierHash),

  % Create an SVG representation for the given hash.
  export_ihier(IHierHash, Dom, Opts),

  if_debug(iotw, end_experiment(IHierHash, NumberOfIPairs)),

  % Run the evaluation.
  (   option(evaluate(false), Opts, false)
  ->  true
  ;   evaluate_inodes(IHierHash)
  ),

  % Done!
  clear_ihiers.





% DEBUG %

%! begin_experiment(
%!   +ISets:ordset(ordset(iri)),
%!   -NumberOfIPairs:integer
%! ) is det.

begin_experiment(ISets, NumberOfIPairs):-
  % Print the number of identity sets.
  length(ISets, NumberOfISets),
  debug(iotw, "There are ~D identity sets.", [NumberOfISets]),

  % Print the number of identity pairs
  % that is expressed by the given collection of identity sets.
  %
  % Notice that not all identity pairs may have been explicit
  % in the original collection of pairs.
  % I.e., in the following conversions, pairs1 and pairs2
  % need not be the same: pairs1 -> sets -> pairs2
  number_of_equivalence_pairs(ISets, NumberOfIPairs),
  debug(iotw, "There are ~D identity pairs.", [NumberOfIPairs]),

  % Print the number of resources.
  maplist(length, ISets, Ns),
  sum_list(Ns, NumberOfResources),
  debug(iotw, "There are ~D resources.", [NumberOfResources]),

  % Print the number of non-pair identity sets.
  % This quantifies the usefulness of using sets instead of pairs.
  exclude(\S^length(S, 2), ISets, ISets0),
  length(ISets0, NumberOfNonpairISets),
  debug(iotw, "Number of non-pair identity sets: ~D.", [NumberOfNonpairISets]).



%! end_experiment(+IHierHash:atom, +NumberOfAllIPairs:integer) is det.

end_experiment(IHierHash, NumberOfAllIPairs1):-
  aggregate_all(
    sum(NumberOfIPairs),
    inode(_, IHierHash, _, _, NumberOfIPairs, _, _, _),
    NumberOfAllIPairs2
  ),
  (   NumberOfAllIPairs1 =:= NumberOfAllIPairs2
  ->  true
  ;   debug(
        iotw,
        "Number of ipairs does not match: ~D and ~D.",
        [NumberOfAllIPairs1,NumberOfAllIPairs2]
      )
  ).
