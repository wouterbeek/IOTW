:- module(
  iotw_export,
  [
    build_vertex/3, % +GraphAlignmentPairsTerm:compound
                    % +IdentityNodeTerm:compound
                    % -VertexTerm:compound
    export_identity_nodes/2 % +GraphAlignmentPairsHash:atom
                            % -SVG:dom
  ]
).

/** <module> IOTW_EXPORT
Exports the results of classifying alignment resource pairs
by the predicates they share.

@author Wouter Beek
@tbd Add rank to the left-hand side of the graphic order using GraphViz edges.
@version 2013/05, 2013/08
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(set_theory)).
:- use_module(gv(gv_file)).
:- use_module(iotw(iotw_inodes)).
:- use_module(library(lists)).
:- use_module(rdf(rdf_name)).
:- use_module(xml(xml_dom)).



%! build_vertex(
%!   +GraphAlignmentPairsTerm:compound,
%!   +IdentityNodeTerm:compound,
%!   -VertexTerm:compound
%! ) is det.
% Exports a single identity node representing a set of predicates
% and the pairs of resources that share those predicates.

build_vertex(GA, IdentityNode, Vertex):-
  GA = graph_alignment(GA_Hash,_,_,_,NumberOfIdentityPairs,_),
  IdentityNode = identity_node(
    GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs
  ),
  Vertex = vertex(GAK_Hash,IdentityNode,VertexAttributes),

  (  InHigher = true
  -> Color = darkgreen
  ;  Color = lightgreen),

  % The key label that describes the key.
  rdf_terms_name(Key, KeyLabel),

  % How many key pairs are identity key pairs?
  % This is the precision of `Key`, defined in the standard way:
  % $Precision(Key) = \frac{\vert Relevant(X) \vert}{\vert Retrieved(X) \vert}$
  %   * $Relevant(X) = (\bigcap Key) \cap \approx$
  %   * $Retrieved(X) = (\bigcap Key)$
  %   * Because all relevant pairs are retrieved,
  %     the relevant and retrieved pairs are the relevant pairs.
  % Notice that the recall is not displayed, since it is always `1.0`.
  (
    nonvar(NumberOfKeyPairs)
  ->
    Precision is NumberOfKeyIdentityPairs / NumberOfKeyPairs,
    format(
      atom(PrecisionLabel),
      ' precision[~d/~d=~2f]',
      [NumberOfKeyIdentityPairs,NumberOfKeyPairs,Precision]
    )
  ;
    PrecisionLabel = ''
  ),

  % How many identity pairs are identity key pairs?
  % @tbd Is there a common name for this metric in the literature?
  Percentage is NumberOfKeyIdentityPairs / NumberOfIdentityPairs,

  % We like our vertex labels complicated...
  format(
    atom(VertexLabel),
    '~w~w identity[~d/~d=~2f]',
    [
      KeyLabel,
      PrecisionLabel,
      NumberOfKeyIdentityPairs,
      NumberOfIdentityPairs,
      Percentage
    ]
  ),
  VertexAttributes =
    [color(Color),label(VertexLabel),shape(rectangle),style(solid)].

%! export_identity_nodes(+GraphAlignmentPairsHash:atom, -SVG:dom) is det.
% Returns the SVG DOM representation of the hierarchy of predicate (sub)sets
% annotated with the number of resource pairs that share those and only those
% predicates.
%
% @tbd Add callback function injection.

export_identity_nodes(GA_Hash, SVG2):-
gtrace,
  export_identity_nodes_(GA_Hash, GIF),
  graph_to_svg_dom([], GIF, dot, SVG1),
  xml_inject_dom_with_attribute(SVG1, node, [onclick='function()'], SVG2).

%! export_identity_nodes_(
%!   +GraphAlignmentPairsHash:atom,
%!   -GIF:compound
%! ) is det.

export_identity_nodes_(GA_Hash, GIF):-
  graph_alignment(GA_Hash,G,A,_,NumberOfIdentityPairs,NumberOfPairs),

  % Extract the ranks that occur in the hierarchy.
  setoff(
    RankNumber,
    (
      identity_node(_GAK_Hash,_GA_Hash,Key,_InHigher,_NumberOfKeyIdentityPairs,_NumberOfKeyPairs),
      cardinality(Key, RankNumber)
    ),
    RankNumbers
  ),

  % Build nodes: build the non-nil nodes.
  findall(
    rank(vertex(RankId,RankId,RankAttrs),V_Terms),
    (
      % The rank.
      member(RankNumber, RankNumbers),
      format(atom(RankId), 'r~w', [RankNumber]),
      atom_number(RankLabel, RankNumber),
      RankAttrs = [label(RankLabel), shape(plaintext)],

      % Consider only keys of the rank length.
      length(Key, RankNumber),
      findall(
        V_Term,
        (
          identity_node(GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs),
          build_vertex(
            graph_alignment(GA_Hash,G,A,_,NumberOfIdentityPairs,NumberOfPairs),
            identity_node(GAK_Hash,GA_Hash,Key,InHigher,NumberOfKeyIdentityPairs,NumberOfKeyPairs),
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
      identity_node(ToId,GA_Hash,ToKey,_,_,_),
      strict_sublist(FromKey, ToKey),
      identity_node(FromId,GA_Hash,FromKey,_,_,_),
      \+ ((
        strict_sublist(MiddleKey, ToKey),
        identity_node(_,GA_Hash,MiddleKey,_,_,_),
        strict_sublist(FromKey, MiddleKey)
      ))
    ),
    Es
  ),

  % Calculate the quality of the rough set, if this is possible.
  (
    possible_to_calculate_quality(GA_Hash)
  ->
    calculate_quality(GA_Hash, Q),
    format(atom(Q_Label), '   Quality: ~e', [Q])
  ;
    Q_Label = ''
  ),

  % Graph properties.
  format(atom(G_Label), 'Name: ~w~w', [G,Q_Label]),
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

