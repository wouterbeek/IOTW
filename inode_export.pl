:- module(
  inode_export,
  [
    export_inodes/3 % +Options:list(nvpair)
                    % +IdentityHierarchyHash:atom
                    % -SVG:dom
  ]
).

/** <module> Export of identity nodes
Exports the results of classifying alignment resource pairs
by the predicates they share.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09, 2013/11
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(gv(gv_file)).
:- use_module(iotw(inode)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(os(run_ext)). % DEB
:- use_module(rdf(rdf_name)).
:- use_module(xml(xml_dom)).

:- debug(iotw_export).



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
    InHigher,
    NumberOfIdPairs,
    NumberOfPairs
  ),

  % Retrieve the number of identity pairs in the parent entity.
  number_of_parent_identity_pairs(Mode, ParentHash, NumberOfParentIdPairs),

  % Vertex color.
  (InHigher == true -> Color = green ; Color = red),

  % Vertex style.
  (Mode == p -> Style = solid ; Style = dashed),

  % The label that describes the shared predicates
  % or the shared predicate-object pairs.
  (
    Mode == p
  ->
    with_output_to(
      atom(SharedLabel),
      print_set([write_method(rdf_term_name)], Shared)
    )
  ;
    with_output_to(
      atom(SharedLabel),
      print_set([write_method(rdf_pair_name)], Shared)
    )
  ),

  % Compose the label that describes this node.
  % Notice that the recall is not displayed, since it is always `1.0`.
  precision_label(NumberOfIdPairs, NumberOfPairs, PrecisionLabel),
  percentage_label(NumberOfIdPairs, NumberOfParentIdPairs, PercentageLabel),
  format(
    atom(V_Label),
    '~w~w identity~w',
    [SharedLabel,PrecisionLabel,PercentageLabel]
  ),

  V_Attrs = [color(Color),label(V_Label),shape(rectangle),style(Style)].

calculate(IHierHash, InHigher, NumberOfPairs):-
  aggregate_all(
    sum(NumberOfPairs_),
    inode(_, _, IHierHash, _, InHigher, _, NumberOfPairs_),
    NumberOfPairs
  ).

calculate_higher(IHierHash, Cardinality):-
  calculate(IHierHash, true, Cardinality).

calculate_lower(IHierHash, Cardinality):-
  calculate(IHierHash, _, Cardinality).

calculate_quality(IHierHash, Quality):-
  calculate_higher(IHierHash, HigherCardinality),
  calculate_lower(IHierHash, LowerCardinality),

  % Make sure we never divide by zero.
  (
    HigherCardinality =:= LowerCardinality
  ->
    Quality = 1.0
  ;
    Quality = HigherCardinality / LowerCardinality
  ).

%! export_inodes(
%!   +Options:list(nvpair),
%!   +IdentityHierarchyHash:atom,
%!   -SVG:dom
%! ) is det.
% Returns the SVG DOM representation of the hierarchy of predicate (sub)sets
% annotated with the number of resource pairs that share those and only those
% predicates.
%
% @tbd Add callback function injection.

export_inodes(O1, IHierHash, SVG2):-
  export_identity_nodes_(O1, IHierHash, GIF),
  graph_to_svg_dom([method(dot)], GIF, SVG1),
  xml_inject_dom_with_attribute(SVG1, node, [onclick='function()'], SVG2),

  % DEB: Aslo export as PDF (in a persistent file).
  (
    option(deb_pdf(true), O1, false)
  ->
    current_date_time(DT),
    absolute_file_name(
      personal(DT),
      PDF_File,
      [access(write),file_type(pdf)]
    ),
    graph_to_gv_file([method(dot),to_file_type(pdf)], GIF, PDF_File),
    open_pdf(PDF_File)
  ;
    true
  ).

%! export_identity_nodes_(
%!   +Options:list(nvpair),
%!   +IdentityHierarchyHash:atom,
%!   -GIF:compound
%! ) is det.

export_identity_nodes_(O, IHierHash, GIF):-
  % Mode `p` constrains the nodes that we find edges for.
  option(granularity(Mode), O, p),
  (Mode == p -> Mode_ = p ; true),

  % Vertices for `po`-nodes.
  % First extract the ranks that occur in the hierarchy.
  % The ranks are the cardinalities of the sets of shared predicates.
  % Ranks are used to align the partitioning subsets is a style similar
  % to a Hasse Diagram.
  setoff(
    RankNumber,
    (
      inode(Mode_, _, IHierHash, SharedPs, _, _, _),
      length(SharedPs, RankNumber)
    ),
    RankNumbers
  ),

  findall(
    rank(vertex(RankId,RankId,RankAttrs),P_V_Terms),
    (
      % We do this for every rank.
      member(RankNumber, RankNumbers),
      format(atom(RankId), 'r~w', [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankAttrs = [label(RankLabel), shape(plaintext)],

      % Consider only those sets of shared predicates that have rank cardinality.
      length(SharedPs, RankNumber),
      findall(
        P_V_Term,
        (
          inode(p, INodeHash, IHierHash, SharedPs, _, _, _),
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
      inode(p, INodeHash, IHierHash, _, _, _, _),
      inode(po, ISubnodeHash, INodeHash, _, _, _, _),
      build_vertex(ISubnodeHash, PO_V_Term)
    ),
    PO_V_Terms
  ),

  % Edges between the identity nodes of the _same_ mode.
  findall(
    edge(FromHash,ToHash,E_Attrs),
    (
      % Find two nodes that are either directly or indirectly related.
      inode(Mode_, FromHash, ParentHash, FromShared, _, _, _),
      inode(Mode_, ToHash, ParentHash, ToShared, _, _, _),
      ord_strict_subset(FromShared, ToShared),

      % There must be no node in between:
      % We only display edges between _directly_ related vertices.
      \+ ((
        inode(Mode_, _, ParentHash, MiddleShared, _, _, _),
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
      inode(p, NodeHash, _, _, _, _, _),
      inode(po, SubnodeHash, NodeHash, _, _, _, _),
      E_Atts = [color(black),style(dashed)]
    ),
    Es2
  ),
  append(Es1, Es2, Es),

  % Graph attributes.
  %quality_label(IHierHash, Q_Label),
  rdf_statistics(triples_by_graph(G,Triples)),
  format(atom(G_Label), 'Graph: ~w.  Triples:~w', [G,Triples]),
  G_Attrs =
    [
      colorscheme(svg),
      charset('UTF-8'),
      fontsize(11.0),
      label(G_Label),
      overlap(false)
    ],

  % The graph compound term.
  GIF = graph(PO_V_Terms, Ranks, Es, [graph_name(G)|G_Attrs]).

number_of_parent_identity_pairs(p, ParentHash, NumberOfParentIdPairs):- !,
  ihier(ParentHash, _, _, _, _, NumberOfParentIdPairs).
number_of_parent_identity_pairs(po, ParentHash, NumberOfParentIdPairs):-
  inode(p, ParentHash, _, _, _, NumberOfParentIdPairs, _).

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
    '[~d/~d=~2f]',
    [NumberOfIdPairs,NumberOfParentIdPairs,Percentage]
  ).

possible_to_calculate(IHierHash, InHigher):-
  forall(
    inode(_, _, IHierHash, _, InHigher, _, NumberOfPairs),
    nonvar(NumberOfPairs)
  ).

possible_to_calculate_higher(IHierHash):-
  possible_to_calculate(IHierHash, true).

possible_to_calculate_lower(IHierHash):-
  possible_to_calculate(IHierHash, _).

possible_to_calculate_quality(IHierHash):-
  possible_to_calculate_higher(IHierHash),
  possible_to_calculate_lower(IHierHash).

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
    ' precision[~d/~d=~2f]',
    [NumberOfIdPairs,NumberOfPairs,Precision]
  ).

%! quality_label(+IHierHash:atom, -Q_Label:atom) is det.
% Calculates the quality of the rough set, if this is possible.

quality_label(IHierHash, Q_Label):-
  possible_to_calculate_quality(IHierHash), !,
  calculate_quality(IHierHash, Q),
  format(atom(Q_Label), '   Quality: ~e', [Q]).
quality_label(_IHierHash, '').

