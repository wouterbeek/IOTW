:- module(
  iotw_export,
  [
    build_vertex/5, % +Assoc:assoc
                    % +Key:ordset(uri)
                    % +NumberOfIdentityPairs:integer
                    % +NumberOfThesePairs:integer
                    % -Node:element
    init_export/0,
    node/5 % ?NodeID:atom,
           % ?Key:ordset(iri),
           % ?NumberOfIdentityPairs:nonneg,
           % ?NumberOfPairs:nonneg,
           % ?InLowerApproximation:boolean
  ]
).

/** <module> IOTW_EXPORT

@author Wouter Beek
@version 2013/05-2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(gv(gv_hash)).
:- use_module(library(assoc)).
:- use_module(rdf(rdf_name)).

%! node(
%!   ?NodeID:atom,
%!   ?Key:ordset(iri),
%!   ?NumberOfIdentityPairs:nonneg,
%!   ?NumberOfPairs:nonneg,
%!   ?InLowerApproximation:boolean
%! ) is nondet.

:- dynamic(node/5).



%! build_vertex(
%!   +Assoc:assoc,
%!   +Key:ordset(uri),
%!   +NumberOfIdentityPairs:integer,
%!   +NumberOfThesePairs:integer,
%!   -Node:element
%! ) is det.
% Exports a single node representing a set of predicates
% and the pairs of resources that share those predicates.

build_vertex(
  Assoc,
  Key,
  NumberOfIdentityPairs,
  NumberOfThesePairs,
  vertex(NodeID,NodeID,NodeAttributes)
):-
  % Create the key label that described the key.
  rdf_terms_name(Key, KeyLabel),

  % Establish the node ID.
  indexed_sha_hash(Key, Hash),
  format(atom(NodeID), 'n~w', [Hash]),

  % Count the identity pairs and percentage.
  unless(
    assoc:get_assoc(Key, Assoc, TheseIdentityPairs),
    TheseIdentityPairs = []
  ),
  cardinality(TheseIdentityPairs, NumberOfTheseIdentityPairs),
  Percentage1 is NumberOfTheseIdentityPairs / NumberOfIdentityPairs,

  % Calculate the percentage of identity pairs relative to all pairs
  % in the partition set.
  Percentage2 is NumberOfTheseIdentityPairs / NumberOfThesePairs,

  % Whether the node belongs to the lower approximation or not.
  (
    NumberOfTheseIdentityPairs =:= NumberOfThesePairs
  ->
    InLowerApproximation = true
  ;
    InLowerApproximation = false
  ),
  assert(
    node(
      NodeID,
      Key,
      NumberOfIdentityPairs,
      NumberOfThesePairs,
      InLowerApproximation
    )
  ),

  % We like our node labels complicated...
  format(
    atom(NodeLabel),
    '~w [~d/~d=~2f] [~d/~d=~2f]',
    [
      KeyLabel,
      NumberOfTheseIdentityPairs,
      NumberOfIdentityPairs,
      Percentage1,
      NumberOfTheseIdentityPairs,
      NumberOfThesePairs,
      Percentage2
    ]
  ),
  NodeAttributes =
    [color(blue),label(NodeLabel),shape(rectangle),style(solid)].

init_export:-
  retractall(node/5).

