:- module(
  inode_export,
  [
    export_identity_nodes/3 % +IdentityHierarchyHash:atom
                            % -SVG:dom
                            % -PDF_File:atom
  ]
).

/** <module> Export of identity nodes
Exports the results of classifying alignment resource pairs
by the predicates they share.

@author Wouter Beek
@tbd Add rank to the left-hand side of the graphic order using GraphViz edges.
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(gv(gv_file)).
:- use_module(iotw(inode)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(os(datetime_ext)).
:- use_module(rdf(rdf_name)).
:- use_module(xml(xml_dom)).

:- debug(iotw_export).



%! build_vertex(
%!   +IdentityHierarchy:compound,
%!   +IdentityNode:compound,
%!   -VertexTerm:compound
%! ) is det.
% Exports a single identity node representing a set of predicates
% and the pairs of resources that share those predicates.

build_vertex(
  ihier(IHierHash,_,_,_,_,NumberOfAllISets,_),
  inode(
    INodeHash,
    IHierHash,
    SharedPs,
    InHigher,
    NumberOfISets,
    NumberOfPairs
  ),
  vertex(INodeHash,INodeHash,V_Attrs)
):-
  (  InHigher = true
  -> Color = green
  ;  Color = red),

  % The key label that describes the key.
  rdf_terms_name(SharedPs, SharedPsLabel),

  % How many key pairs are identity key pairs?
  % This is the precision of `Key`, defined in the standard way:
  % $Precision(Key) = \frac{\vert Relevant(X) \vert}{\vert Retrieved(X) \vert}$
  %   * $Relevant(X) = (\bigcap Key) \cap \approx$
  %   * $Retrieved(X) = (\bigcap Key)$
  %   * Because all relevant pairs are retrieved,
  %     the relevant and retrieved pairs are the relevant pairs.
  % Notice that the recall is not displayed, since it is always `1.0`.
  (
    nonvar(NumberOfPairs)
  ->
    % @tbd This is no longer correct: sets/pairs.
    Precision is NumberOfISets / NumberOfPairs,
    format(
      atom(PrecisionLabel),
      ' precision[~d/~d=~2f]',
      [NumberOfISets,NumberOfPairs,Precision]
    )
  ;
    PrecisionLabel = ''
  ),

  % How many identity pairs are identity key pairs?
  % @tbd Is there a common name for this metric in the literature?
  Percentage is NumberOfISets / NumberOfAllISets,

  % We like our vertex labels complicated...
  format(
    atom(V_Label),
    '~w~w identity[~d/~d=~2f]',
    [
      SharedPsLabel,
      PrecisionLabel,
      NumberOfISets,
      NumberOfAllISets,
      Percentage
    ]
  ),
  V_Attrs =
    [color(Color),label(V_Label),shape(rectangle),style(solid)].

calculate(IHierHash, InHigher, Cardinality):-
  aggregate(
    sum(N),
    inode(_,IHierHash,_,InHigher,_,N),
    Cardinality
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

%! export_identity_nodes(
%!   +IdentityHierarchyHash:atom,
%!   -SVG:dom,
%!   -PDF_File:atom
%! ) is det.
% Returns the SVG DOM representation of the hierarchy of predicate (sub)sets
% annotated with the number of resource pairs that share those and only those
% predicates.
%
% @tbd Add callback function injection.

export_identity_nodes(IHierHash, SVG2, PDF_File):-
  export_identity_nodes_(IHierHash, GIF),
  graph_to_svg_dom([], GIF, dot, SVG1),
  xml_inject_dom_with_attribute(SVG1, node, [onclick='function()'], SVG2),

  % DEB: Aslo export as PDF (in a persistent file).
  (
    debug(iotw_export)
  ->
    current_date_time(DT),
    absolute_file_name(
      personal(DT),
      PDF_File,
      [access(write),file_type(pdf)]
    ),
    graph_to_gv_file([], GIF, dot, pdf, PDF_File)
  ;
    true
  ).

%! export_identity_nodes_(+IdentityHierarchyHash:atom, -GIF:compound) is det.

export_identity_nodes_(IHierHash, GIF):-
  ihier(
    IHierHash,
    G,
    IdSets,
    P_Assoc,
    PPO_Assoc,
    NumberOfAllIdentitySets,
    NumberOfAllPairs
  ),

  % Extract the ranks that occur in the hierarchy.
  % The ranks are the cardinalities of the sets of shared predicates.
  % Ranks are used to align the partitioning subsets is a style similar
  % to a Hasse Diagram.
  setoff(
    RankNumber,
    (
      inode(_, IHierHash, SharedPs, _, _, _),
      length(SharedPs, RankNumber)
    ),
    RankNumbers
  ),
  % Build the identity nodes.
  findall(
    rank(vertex(RankId,RankId,RankAttrs),V_Terms),
    (
      % We do this for every rank.
      member(RankNumber, RankNumbers),
      format(atom(RankId), 'r~w', [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankAttrs = [label(RankLabel), shape(plaintext)],

      % Consider only those sets of shared predicates that are of
      % the given rank cardinality.
      length(SharedPs, RankNumber),
      findall(
        V_Term,
        (
          inode(
            INodeHash,
            IHierHash,
            SharedPs,
            InHigher,
            NumberOfIdentitySets,
            NumberOfPairs
          ),
          build_vertex(
            ihier(
              IHierHash,
              G,
              IdSets,
              P_Assoc,
              PPO_Assoc,
              NumberOfAllIdentitySets,
              NumberOfAllPairs
            ),
            inode(
              INodeHash,
              IHierHash,
              SharedPs,
              InHigher,
              NumberOfIdentitySets,
              NumberOfPairs
            ),
            V_Term
          )
        ),
        V_Terms
      )
    ),
    Ranks
  ),

  % Edges
  E_Attrs = [color(black),style(solid)],
  findall(
    edge(FromId,ToId,E_Attrs),
    (
      inode(ToId,IHierHash,ToPs,_,_,_),
      strict_sublist(FromPs, ToPs),
      inode(FromId,IHierHash,FromPs,_,_,_),
      \+ ((
        strict_sublist(MiddlePs, ToPs),
        inode(_,IHierHash,MiddlePs,_,_,_),
        strict_sublist(FromPs, MiddlePs)
      ))
    ),
    Es
  ),

  % Calculate the quality of the rough set, if this is possible.
  (
    possible_to_calculate_quality(IHierHash)
  ->
    calculate_quality(IHierHash, Q),
    format(atom(Q_Label), '   Quality: ~e', [Q])
  ;
    Q_Label = ''
  ),

  % Graph properties.
  rdf_statistics(triples_by_graph(G,Triples)),
  format(atom(G_Label), 'Graph: ~w.  Triples:~w~w', [G,Triples,Q_Label]),
  G_Attrs =
    [
      colorscheme(svg),
      charset('UTF-8'),
      fontsize(11.0),
      label(G_Label),
      overlap(false)
    ],

  % The graph compound term.
  GIF = graph([], Ranks, Es, [graph_name(G)|G_Attrs]).

possible_to_calculate(IHierHash, InHigher):-
  forall(
    inode(_,IHierHash,_,InHigher,_,NumberOfPairs),
    nonvar(NumberOfPairs)
  ).

possible_to_calculate_higher(IHierHash):-
  possible_to_calculate(IHierHash, true).

possible_to_calculate_lower(IHierHash):-
  possible_to_calculate(IHierHash, _).

possible_to_calculate_quality(IHierHash):-
  possible_to_calculate_higher(IHierHash),
  possible_to_calculate_lower(IHierHash).

