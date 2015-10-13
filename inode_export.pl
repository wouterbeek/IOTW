:- module(
  inode_export,
  [
    export_ihier_as_gif/3, % +IdentityHierarchyHash:atom
                           % -Gif:compound
                           % +Options:list(compound)
    export_ihier_as_svg/3 % +IdentityHierarchyHash:atom
                          % -SVG:dom
                          % +Options:list(compound)
  ]
).

/** <module> Export of identity nodes

Exports the results of classifying alignment resource pairs
by the predicates they share.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(gv/gv_dom)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/datetime_file)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xml/xml_dom)).

:- use_module(inode).

:- predicate_options(export_ihier_as_svg/3, 3, [
     pass_to(export_ihier_as_gif/3, 3)
   ]).
:- predicate_options(export_ihier_as_gif/3, 3, [
     evaluate(+boolean),
     granularity(+oneof([p,po]))
   ]).



%! build_vertex(+IdentityNodeHash:atom, -VertexTerm:compound) is det.
% Exports a single identity node representing a set of predicates
% and the pairs of resources that share those predicates.

build_vertex(NodeHash, vertex(NodeHash,NodeHash,V_Attrs)):-
  % Retrieve the inode based on the given hash.
  inode(
    Mode,
    NodeHash,
    ParentHash,
    Shared,
    Approx,
    NumberOfIdPairs,
    _,
    NumberOfPairs,
    _
  ),

  % Retrieve the number of identity pairs in the parent entity.
  number_of_parent_identity_pairs(Mode, ParentHash, NumberOfParentIdPairs),

  % Vertex color.
  (Approx == lower -> Color = green ; Approx == higher -> Color = red),

  % Vertex style.
  (Mode == p -> Style = solid ; Style = dashed),

  % The label that describes the shared predicates
  % or the shared predicate-object pairs.
  (   Mode == p
  ->  string_phrase(set(rdf_print_term, Shared), SharedLabel)
  ;   string_phrase(set(pair(rdf_print_term), Shared), SharedLabel)
  ),

  % Compose the label that describes this node.
  % Notice that the recall is not displayed, since it is always `1.0`.
  precision_label(NumberOfIdPairs, NumberOfPairs, PrecisionLabel),
  percentage_label(NumberOfIdPairs, NumberOfParentIdPairs, PercentageLabel),
  format(
    string(V_Label),
    "~w\n~w\nidentity~w",
    [SharedLabel,PrecisionLabel,PercentageLabel]
  ),

  V_Attrs = [color(Color),label(V_Label),shape(rectangle),style(Style)].

calculate(IHierHash, Approx, NumberOfPairs):-
  aggregate_all(
    sum(NumberOfPairs_),
    inode(_, _, IHierHash, _, Approx, _, _, NumberOfPairs_, _),
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

%! export_ihier_as_svg(
%!   +IdentityHierarchyHash:atom,
%!   -SVG:dom,
%!   +Options:list(compound)
%! ) is det.
% Returns the SVG DOM representation of the hierarchy of predicate (sub)sets
% annotated with the number of resource pairs that share those and only those
% predicates.
%
% @tbd Add callback function injection.

export_ihier_as_svg(IHierHash, Dom, Opts):-
  export_ihier_as_gif(IHierHash, ExportG, Opts),
  gv_dom(ExportG, Dom0, Opts),
  xml_inject_dom_with_attribute(Dom0, node, [onclick='function()'], Dom),

  % DEB: Aslo export as PDF (in a persistent file).
  (   option(deb_pdf(true), Opts, false)
  ->  create_datetime_file(File),
      gv_export(ExportG, File, [method(dot),output(pdf)]),
      open_pdf(File)
  ;   true
  ).

%! export_ihier_as_gif(
%!   +IdentityHierarchyHash:atom,
%!   -Gif:compound,
%!   +Options:list(compound)
%! ) is det.

export_ihier_as_gif(IHierHash, Gif, Opts):-
  % Mode `p` constrains the nodes that we find edges for.
  option(granularity(Mode), Opts, p),
  % @tbd So... Mode_ should be var in case Mode=po,
  %      thereby matching both p and po inodes?
  (Mode == p -> Mode_ = p ; true),

  % Vertices for `po`-nodes.
  % First extract the ranks that occur in the hierarchy.
  % The ranks are the cardinalities of the sets of shared predicates.
  % Ranks are used to align the partitioning subsets is a style similar
  % to a Hasse Diagram.
  aggregate_all(
    set(RankNumber),
    (
      inode(Mode_, _, IHierHash, SharedPs, _, _, _, _, _),
      length(SharedPs, RankNumber)
    ),
    RankNumbers
  ),

  % Construct the vertice terms per rank.
  findall(
    rank(vertex(RankId,RankId,RankAttrs),P_V_Terms),
    (
      % We do this for every rank.
      member(RankNumber, RankNumbers),
      format(atom(RankId), 'r~w', [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankAttrs = [label(RankLabel),shape(plaintext)],

      % Consider only those sets of shared predicates
      % whose cardinality is the given rank.
      length(SharedPs, RankNumber),
      findall(
        P_V_Term,
        (
          inode(p, INodeHash, IHierHash, SharedPs, _, _, _, _, _),
          build_vertex(INodeHash, P_V_Term)
        ),
        P_V_Terms
      )
    ),
    Ranks
  ),

  % Vertices for `po`-nodes.
  findall(
    PO_V_Term,
    (
      % No vertices for `po` nodes are created in `p`-mode.
      Mode \== p,
      inode(p, INodeHash, IHierHash, _, _, _, _, _, _),
      inode(po, ISubnodeHash, INodeHash, _, _, _, _, _, _),
      build_vertex(ISubnodeHash, PO_V_Term)
    ),
    PO_V_Terms
  ),

  % Edges between the identity nodes of the _same_ mode.
  findall(
    edge(FromHash,ToHash,E_Attrs),
    (
      % Find two nodes that are either directly or indirectly related.
      inode(Mode_, FromHash, ParentHash, FromShared, _, _, _, _, _),
      inode(Mode_, ToHash, ParentHash, ToShared, _, _, _, _, _),
      ord_strict_subset(FromShared, ToShared),

      % There must be no node in between:
      % We only display edges between _directly_ related vertices.
      \+ ((
        inode(Mode_, _, ParentHash, MiddleShared, _, _, _, _, _),
        ord_strict_subset(FromShared, MiddleShared),
        ord_strict_subset(MiddleShared, ToShared)
      )),

      % Base the edge style on the identity nodes mode.
      (Mode_ == p -> Style = solid ; Style = dotted),

      % Edge attributes.
      E_Attrs = [color(black),style(Style)]
    ),
    Es1
  ),
  % Edges between the identity nodes of the _different_ modes.
  findall(
    edge(SubnodeHash,NodeHash,E_Atts),
    (
      inode(p, NodeHash, _, _, _, _, _, _, _),
      inode(po, SubnodeHash, NodeHash, _, _, _, _, _, _),
      E_Atts = [color(black),style(dashed)]
    ),
    Es2
  ),
  append(Es1, Es2, Es),

  % Graph attributes.
  quality_label(IHierHash, Q_Label),
  rdf_statistics(triples_by_graph(G,Triples)),
  format(atom(G_Label), 'Graph:~w\tTriples:~:d~w', [G,Triples,Q_Label]),
  G_Attrs =
    [
      colorscheme(svg),
      charset('UTF-8'),
      fontsize(11.0),
      label(G_Label),
      overlap(false)
    ],

  % The graph compound term.
  Gif = graph(PO_V_Terms, Ranks, Es, [graph_name(G)|G_Attrs]).

number_of_parent_identity_pairs(p, ParentHash, NumberOfParentIdPairs):- !,
  ihier(ParentHash, _, _, _, _, NumberOfParentIdPairs).
number_of_parent_identity_pairs(po, ParentHash, NumberOfParentIdPairs):-
  inode(p, ParentHash, _, _, _, NumberOfParentIdPairs, _, _, _).

ord_strict_subset(Sub, Super):-
  ord_subset(Sub, Super),
  Sub \== Super.

%! percentage_label(
%!   +NumberOfIdentityPairs:nonneg,
%!   +NumberOfParentIdentityPairs:nonneg,
%!   -PercentageLabel:atom
%! ) is det.
% Returns an atomic representation of the percentage of the parent's
% identity relation that is covered by a specific node.

percentage_label(NumberOfIdPairs, NumberOfParentIdPairs, PercentageLabel):-
  Percentage is NumberOfIdPairs / NumberOfParentIdPairs,
  format(
    atom(PercentageLabel),
    '[~:d/~:d=~2f]',
    [NumberOfIdPairs,NumberOfParentIdPairs,Percentage]
  ).

possible_to_calculate(IHierHash, Approx):-
  forall(
    inode(_, _, IHierHash, _, Approx, _, _, NumberOfPairs, _),
    nonvar(NumberOfPairs)
  ).

possible_to_calculate_quality(IHierHash):-
  possible_to_calculate(IHierHash, higher),
  possible_to_calculate(IHierHash, lower).

%! precision_label(
%!   +NumberOfIdentityPairs:nonneg,
%!   +NumberOfPairs:nonneg,
%!   -PrecisionLabel:atom
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

precision_label(_NumberOfIdPairs, NumberOfPairs, PrecisionLabel):-
  var(NumberOfPairs), !,
  PrecisionLabel = ''.
precision_label(NumberOfIdPairs, NumberOfPairs, PrecisionLabel):-
  Precision is NumberOfIdPairs / NumberOfPairs,
  format(
    atom(PrecisionLabel),
    ' precision[~:d/~:d=~2f]',
    [NumberOfIdPairs,NumberOfPairs,Precision]
  ).

%! quality_label(+IHierHash:atom, -Q_Label:atom) is det.
% Calculates the quality of the rough set, if this is possible.

quality_label(IHierHash, Q_Label):-
  possible_to_calculate_quality(IHierHash), !,
  calculate_quality(IHierHash, Q),
  format(atom(Q_Label), '\tQuality:~2f', [Q]).
quality_label(_IHierHash, '').
