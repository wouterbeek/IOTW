:- module(
  inode_export,
  [
    export_ihier/3 % +IdentityHierarchyHash:atom
                   % -Dom:list(compound)
                   % +Options:list(compound)
  ]
).

/** <module> Export of identity nodes

Exports the results of classifying alignment resource pairs
by the predicates they share.

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(gv/gv_dom)).
:- use_module(library(gv/gv_file)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/datetime_file)).
:- use_module(library(os/pdf)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_stat)).
:- use_module(library(set/set_ext)).
:- use_module(library(xml/xml_dom)).

:- use_module(iotw(inode)).
:- use_module(iotw(iotw_generics)).

:- predicate_options(export_ihier/3, 3, [
     pdf(+boolean),
     pass_to(gv_dom/3, 3),
     pass_to(gv_export/3, 3)
   ]).





%! build_vertex(+IdentityNodeHash:atom, -VertexTerm:compound) is det.
% Exports a single identity node representing a set of predicates
% and the pairs of resources that share those predicates.

build_vertex(NodeHash, vertex(NodeId,VAttrs)):-
  % Retrieve the inode based on the given hash.
  inode(
    NodeHash,
    ParentHash,
    Shared,
    Approx,
    NumberOfIdPairs,
    _,
    NumberOfPairs,
    _
  ),

  % GraphViz node ID's are not allowed to start with a digit.
  hash_id(NodeHash, NodeId),
  
  % Retrieve the number of identity pairs in the parent entity.
  number_of_parent_identity_pairs(ParentHash, NumberOfParentIdPairs),

  % Vertex color.
  (Approx == lower -> VColor = green ; Approx == higher -> VColor = red),

  % The label that describes the shared predicates
  % or the shared predicate-object pairs.
  string_phrase(set(q_print_term, Shared), SharedLabel),

  % Compose the label that describes this node.
  % Notice that the recall is not displayed, since it is always `1.0`.
  precision_label(NumberOfIdPairs, NumberOfPairs, PrecisionLabel),
  percentage_label(NumberOfIdPairs, NumberOfParentIdPairs, PercentageLabel),
  format(
    string(VLabel),
    "~w\n~w\nidentity~w",
    [SharedLabel,PrecisionLabel,PercentageLabel]
  ),

  VAttrs = [color(VColor),label(VLabel),shape(rectangle),style(solid)].

calculate(IHierHash, Approx, NumberOfPairs):-
  aggregate_all(
    sum(NumberOfPairs_),
    inode(_, IHierHash, _, Approx, _, _, NumberOfPairs_, _),
    NumberOfPairs
  ).

calculate_quality(IHierHash, Quality):-
  calculate(IHierHash, higher, HigherCardinality),
  calculate(IHierHash, lower, LowerCardinality),

  % Make sure we never divide by zero.
  (   HigherCardinality =:= LowerCardinality
  ->  Quality = 1.0
  ;   Quality = LowerCardinality / HigherCardinality
  ).

%! export_ihier(
%!   +IdentityHierarchyHash:atom,
%!   -SVG:dom,
%!   +Options:list(compound)
%! ) is det.
% Returns the SVG DOM representation of the hierarchy of predicate (sub)sets
% annotated with the number of resource pairs that share those and only those
% predicates.
%
% Options:
%   * pdf(+boolean)
%     Whether or not a PDF file of the export is created.
%     Default is `false`.

export_ihier(IHierHash, Dom, Opts):-
  export_ihier_graph(IHierHash, ExportG),
  gv_dom(ExportG, Dom, Opts),
  
  (   option(pdf(true), Opts, false)
  ->  create_datetime_file(File),
      merge_options([method(dot),output(pdf)], Opts, PdfOpts),
      gv_export(ExportG, File, PdfOpts),
      open_pdf(File)
  ;   true
  ).

%! export_ihier_graph(+IdentityHierarchyHash:atom, -Gif:compound) is det.

export_ihier_graph(IHierHash, Gif):-
  % First extract the ranks that occur in the hierarchy.
  % The ranks are the cardinalities of the sets of shared predicates.
  % Ranks are used to align the partitioning subsets is a style similar
  % to a Hasse Diagram.
  aggregate_all(
    set(RankNumber),
    (
      inode(_, IHierHash, SharedPs, _, _, _, _, _),
      length(SharedPs, RankNumber)
    ),
    RankNumbers
  ),

  % Construct the vertice terms per rank.
  findall(
    vertex(RankId,RankAttrs)-VTerms,
    (
      % We do this for every rank.
      member(RankNumber, RankNumbers),
      format(string(RankId), "r~w", [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankAttrs = [label(RankLabel),shape(plaintext)],

      % Consider only those sets of shared predicates
      % whose cardinality is the given rank.
      length(SharedPs, RankNumber),
      findall(
        VTerm,
        (
          inode(INodeHash, IHierHash, SharedPs, _, _, _, _, _),
          build_vertex(INodeHash, VTerm)
        ),
        VTerms
      )
    ),
    Ranks
  ),

  % Edges between the identity nodes.
  findall(
    edge(FromId,ToId,EAttrs),
    (
      % Find two nodes that are either directly or indirectly related.
      inode(FromHash, ParentHash, FromShared, _, _, _, _, _),
      inode(ToHash, ParentHash, ToShared, _, _, _, _, _),
      strict_subset(FromShared, ToShared),

      % There must be no node in between:
      % We only display edges between _directly_ related vertices.
      \+ ((
        inode(_, ParentHash, MiddleShared, _, _, _, _, _),
        strict_subset(FromShared, MiddleShared),
        strict_subset(MiddleShared, ToShared)
      )),
      maplist(hash_id, [FromHash,ToHash], [FromId,ToId]),
      EAttrs = [color(black),style(solid)]
    ),
    Es1
  ),
  % Edges between the identity nodes of the _different_ modes.
  findall(
    edge(SubnodeId,NodeId,EAtts),
    (
      inode(NodeHash, _, _, _, _, _, _, _),
      inode(SubnodeHash, NodeHash, _, _, _, _, _, _),
      maplist(hash_id, [SubnodeHash,NodeHash], [SubnodeId,NodeId]),
      EAtts = [color(black),style(dashed)]
    ),
    Es2
  ),
  append(Es1, Es2, Es),

  % Graph attributes.
  quality_label(IHierHash, QLabel),
  ihier(G, IHierHash, _, _, _),
  rdf_number_of_triples(G, N),
  format(string(GLabel), "Graph:~w\tTriples:~:d~w", [G,N,QLabel]),
  GAttrs =
    [
      colorscheme(svg),
      charset('UTF-8'),
      fontsize(11.0),
      label(GLabel),
      overlap(false)
    ],

  % The graph compound term.
  Gif = graph(VTerms, Ranks, Es, GAttrs).



%! number_of_parent_identity_pairs(+Hash:atom, -NumberOfPairs:nonneg) is det.
% Returns the number of pairs for the given hash.
% How to perform the lookup depends on the mode (`p' or `po').

number_of_parent_identity_pairs(Hash, NumberOfPairs):- !,
  ihier(Hash, _, _, _, NumberOfPairs).



%! percentage_label(
%!   +NumberOfChildPairs:nonneg,
%!   +NumberOfParentPairs:nonneg,
%!   -Label:string
%! ) is det.
% Returns an label showing the percentage of the parent's
% identity relation that is covered by a specific node.

percentage_label(ChildPairs, ParentPairs, Label):-
  Perc is ChildPairs / ParentPairs,
  format(string(Label), "[~D/~D=~2f]", [ChildPairs,ParentPairs,Perc]).



%! can_calculate(+IHierHash:atom, +Approximation:oneof([high,low])) is semidet.
% Succeeds if the given Approximation of the size
% of the given identity hierarchy is known.

can_calculate(IHierHash, Approx):-
  forall(
    inode(_, IHierHash, _, Approx, _, _, NumberOfPairs, _),
    nonvar(NumberOfPairs)
  ).



%! can_calculate_quelity(+IHierHash:atom) is semidet.
% Succeeds if the quality of the given identity hierarchy can be calculated.

can_calculate_quality(IHierHash):-
  can_calculate(IHierHash, high),
  can_calculate(IHierHash, low).



%! precision_label(
%!   +NumberOfIdentityPairs:nonneg,
%!   +NumberOfPairs:nonneg,
%!   -Label:string
%! ) is det.
% How many of the pairs that are characterized by this inode
% are actually identity pairs? This is the *precision* of this inode.
% It is defined in the standard way:
%
% $Precision(X) = \frac{\vert Relevant(X) \vert}{\vert Retrieved(X) \vert}$
%   * $Relevant(X) = (\bigcap X) \cap \approx$
%   * $Retrieved(X) = (\bigcap X)$
%   * Because all relevant pairs are retrieved,
%     the relevant and retrieved pairs are the relevant pairs.

precision_label(_, NumberOfPairs, ""):-
  var(NumberOfPairs), !.
precision_label(NumberOfIdPairs, NumberOfPairs, Label):-
  Precision is NumberOfIdPairs / NumberOfPairs,
  format(
    string(Label),
    " precision[~D/~D=~2f]",
    [NumberOfIdPairs,NumberOfPairs,Precision]
  ).



%! quality_label(+IHierHash:atom, -Label:string) is det.
% Calculates the quality of the rough set, if this is possible.

quality_label(IHierHash, Label):-
  can_calculate_quality(IHierHash), !,
  calculate_quality(IHierHash, Q),
  format(string(Label), "\tQuality:~2f", [Q]).
quality_label(_, "").



%! hash_id(+Hash:atom, -Id:atom) is det.

hash_id(Hash, Id):-
  atom_concat(n, Hash, Id).
