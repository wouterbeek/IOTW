:- module(
  iotw_relatedness,
  [
    export_shared_properties/2, % +Graph:atom
                                % +Assoc
    export_shared_properties/3, % +Out:oneof([atom,stream])
                                % +Graph:atom
                                % +Assoc
    rdf_shared_properties/1, % +Graph:atom
    rdf_shared_properties/2 % +Graph:atom
                            % -Assoc
  ]
).

/** <module>

@tbd Ranked output:

==
digraph onto_126 {
  {
    rank=same;
    0 [shape="plaintext"];
    node_n0 [color="black", label="{} (65%)", shape="rectangle", style="solid"];
  }

  {
    rank=same;
    1 [shape="plaintext"];
    node_n24 [color="blue", label="< rdf:type > (92%)", shape="rectangle", style="solid"];
  }

  {
    rank=same;
    2 [shape="plaintext"];
    node_n5 [color="blue", label="< IIMBTBOX:calling_code, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n25 [color="blue", label="< rdf:type, rdfs:subClassOf > (0%)", shape="rectangle", style="solid"];
    node_n26 [color="blue", label="< rdf:type, rdfs:subPropertyOf > (0%)", shape="rectangle", style="solid"];
    node_n9 [color="blue", label="< IIMBTBOX:currency, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n13 [color="blue", label="< IIMBTBOX:featured_by, rdf:type > (1%)", shape="rectangle", style="solid"];
    node_n16 [color="blue", label="< IIMBTBOX:featuring, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n18 [color="blue", label="< IIMBTBOX:filmed_in, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n19 [color="blue", label="< IIMBTBOX:form_of_government, rdf:type > (1%)", shape="rectangle", style="solid"];
    node_n21 [color="blue", label="< IIMBTBOX:gender, rdf:type > (5%)", shape="rectangle", style="solid"];
    node_n22 [color="blue", label="< IIMBTBOX:mainly_spoken_in, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n23 [color="blue", label="< IIMBTBOX:name, rdf:type > (0%)", shape="rectangle", style="solid"];
  }

  {
    rank=same;
    3 [shape="plaintext"];
    node_n2 [color="blue", label="< IIMBTBOX:born_in, IIMBTBOX:gender, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n3 [color="blue", label="< IIMBTBOX:calling_code, IIMBTBOX:currency, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n4 [color="blue", label="< IIMBTBOX:calling_code, IIMBTBOX:form_of_government, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n7 [color="blue", label="< IIMBTBOX:created_by, IIMBTBOX:gender, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n8 [color="blue", label="< IIMBTBOX:currency, IIMBTBOX:form_of_government, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n11 [color="blue", label="< IIMBTBOX:featured_by, IIMBTBOX:gender, rdf:type > (1%)", shape="rectangle", style="solid"];
    node_n12 [color="blue", label="< IIMBTBOX:featured_by, IIMBTBOX:name, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n15 [color="blue", label="< IIMBTBOX:featuring, IIMBTBOX:name, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n17 [color="blue", label="< IIMBTBOX:filmed_in, IIMBTBOX:shot_in, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n20 [color="blue", label="< IIMBTBOX:gender, IIMBTBOX:religion, rdf:type > (0%)", shape="rectangle", style="solid"];
  }

  {
    rank=same;
    4 [shape="plaintext"];
    node_n1 [color="blue", label="< IIMBTBOX:article, IIMBTBOX:form_of_government, IIMBTBOX:name, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n6 [color="blue", label="< IIMBTBOX:created_by, IIMBTBOX:featured_by, IIMBTBOX:gender, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n10 [color="blue", label="< IIMBTBOX:featured_by, IIMBTBOX:featuring, IIMBTBOX:name, rdf:type > (0%)", shape="rectangle", style="solid"];
    node_n14 [color="blue", label="< IIMBTBOX:featuring, IIMBTBOX:filmed_in, IIMBTBOX:shot_in, rdf:type > (0%)", shape="rectangle", style="solid"];
  }

  0 -> 1;
  1 -> 2;
  2 -> 3;
  3 -> 4;

  node_n5 -> node_n3 [color="black", style="solid"];
  node_n5 -> node_n4 [color="black", style="solid"];
  node_n7 -> node_n6 [color="black", style="solid"];
  node_n9 -> node_n3 [color="black", style="solid"];
  node_n9 -> node_n8 [color="black", style="solid"];
  node_n11 -> node_n6 [color="black", style="solid"];
  node_n12 -> node_n10 [color="black", style="solid"];
  node_n13 -> node_n11 [color="black", style="solid"];
  node_n13 -> node_n12 [color="black", style="solid"];
  node_n15 -> node_n10 [color="black", style="solid"];
  node_n16 -> node_n14 [color="black", style="solid"];
  node_n16 -> node_n15 [color="black", style="solid"];
  node_n17 -> node_n14 [color="black", style="solid"];
  node_n18 -> node_n17 [color="black", style="solid"];
  node_n19 -> node_n1 [color="black", style="solid"];
  node_n19 -> node_n4 [color="black", style="solid"];
  node_n19 -> node_n8 [color="black", style="solid"];
  node_n21 -> node_n2 [color="black", style="solid"];
  node_n21 -> node_n7 [color="black", style="solid"];
  node_n21 -> node_n11 [color="black", style="solid"];
  node_n21 -> node_n20 [color="black", style="solid"];
  node_n23 -> node_n1 [color="black", style="solid"];
  node_n23 -> node_n12 [color="black", style="solid"];
  node_n23 -> node_n15 [color="black", style="solid"];
  node_n24 -> node_n5 [color="black", style="solid"];
  node_n24 -> node_n9 [color="black", style="solid"];
  node_n24 -> node_n13 [color="black", style="solid"];
  node_n24 -> node_n16 [color="black", style="solid"];
  node_n24 -> node_n18 [color="black", style="solid"];
  node_n24 -> node_n19 [color="black", style="solid"];
  node_n24 -> node_n21 [color="black", style="solid"];
  node_n24 -> node_n22 [color="black", style="solid"];
  node_n24 -> node_n23 [color="black", style="solid"];
  node_n24 -> node_n25 [color="black", style="solid"];
  node_n24 -> node_n26 [color="black", style="solid"];

  charset="UTF-8"
  fontsize="11.0"
  label="onto_126"
  overlap="false"
}
==

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(meta_ext)).
:- use_module(generics(os_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(standards(graphviz)).



export_shared_properties(Graph, Assoc):-
  absolute_file_name(data(Graph), File, [access(write), file_type(graphviz)]),
  export_shared_properties(File, Graph, Assoc).

export_shared_properties(Stream, Graph, Assoc):-
  is_stream(Stream),
  !,

  % All pairs
  rdf_subjects(Graph, Subjects),
  cardinality(Subjects, NumberOfSubjects),
  NumberOfPairs is NumberOfSubjects ** 2,

  % 1. The number of reflexive pairs is the number of subjects.

  % 2. The number of non-nil and non-reflexive pairs is derived from
  %    the association list.
  assoc_to_values(Assoc, Sets),
  maplist(cardinality, Sets, Cardinalities),
  sum_list(Cardinalities, NumberOfUsefulPairs),

  % 3. The number of nil pairs is the number of pairs, minus the number
  %    of reflexive pairs, minus the number of useful pairs.
  NumberOfNilPairs is NumberOfPairs - NumberOfSubjects - NumberOfUsefulPairs,

  % Nodes: Nil node
  NilPercentage is NumberOfNilPairs / NumberOfPairs * 100,
  NilNode = node(n0, NilNodeAttributes),
  format(
    atom(NilNodeLabel),
    '{} (~0f%) (~w)',
    [NilPercentage, NumberOfNilPairs]
  ),
  NilNodeAttributes =
    [color(black), label(NilNodeLabel), shape(rectangle), style(solid)],

  % Nodes: Non-nil nodes
  assoc_to_keys(Assoc, Keys),
  findall(
    node(NodeID, NodeAttributes),
    (
      nth1(I, Keys, Key),
      rdf_resource_naming(Key, KeyLabel),
      % Retrieve the ordered set, not on of its members!
      assoc:get_assoc(Key, Assoc, Values),
      cardinality(Values, NumberOfValues),
      Percentage is NumberOfValues / NumberOfUsefulPairs * 100,
      format(atom(NodeID), 'n~w', [I]),
      format(
        atom(NodeLabel),
        '~w (~0f%) (~w)',
        [KeyLabel, Percentage, NumberOfValues]
      ),
      NodeAttributes =
        [color(blue), label(NodeLabel), shape(rectangle), style(solid)],
      parse_attributes_graphviz(node, NodeAttributes)
    ),
    NonNilNodes
  ),

  % Edges: Nil-edges
  EdgeAttributes = [color(black), style(solid)],
  parse_attributes_graphviz(edge, EdgeAttributes),
  findall(
    edge(n0, ToNode, EdgeAttributes),
    (
      member(Key, Keys),
      assoc:get_assoc(Key, Assoc, Values),
      cardinality(Values, 1),
      nth1(I, Keys, Key),
      format(atom(ToNode), 'n~w', [I])
    ),
    NilEdges
  ),

  % Edges: Non-nil edges
  findall(
    edge(FromNode, ToNode, EdgeAttributes),
    (
      member(Key1, Key2, Keys),
      % Only strict subsets.
      Key1 \== Key2,
      ord_subset(Key1, Key2),
      % Only direct subset links (the transitive closure is not drawn).
      \+ ((
        member(Key3, Keys),
        Key3 \== Key1,
        Key3 \== Key2,
        ord_subset(Key1, Key3),
        ord_subset(Key3, Key2)
      )),
      nth1(I, Keys, Key1),
      format(atom(FromNode), 'n~w', [I]),
      nth1(J, Keys, Key2),
      format(atom(ToNode), 'n~w', [J])
    ),
    NonNilEdges
  ),

  append(NilEdges, NonNilEdges, Edges),

  % Graph properties
  GraphAttributes =
    [
      charset('UTF-8'),
      fontsize(11.0),
      label(Graph),
      overlap(false)
    ],
  parse_attributes_graphviz(graph, GraphAttributes),

  stream_graphviz(
    Stream,
    graph([NilNode | NonNilNodes], Edges, GraphAttributes)
  ).
export_shared_properties(File, Graph, Assoc):-
  access_file(File, write),
  !,
  open(File, write, Stream),
  export_shared_properties(Stream, Graph, Assoc),
  close(Stream),
  once(file_type_alternative(File, pdf, PDF_File)),
  convert_graphviz(File, dot, pdf, PDF_File),
  open_pdf(PDF_File).

rdf_shared_properties(Graph):-
  rdf_shared_properties(Graph, Assoc),
  export_shared_properties(Graph, Assoc).

%! rdf_shared_properties(+Graph:atom, -Assoc) is det.
% Loads the pairs that share the same properties into an association list
% with ordsets.
%
% # Example
%
% Given the following 4 triples:
% ==
% <S1, P, O12>
% <S2, P, O12>
% <S3, P, O34>
% <S4, P, O34>
% ==
%
% The association list would be:
% ==
% <{P},{<S1,S2>,<S3,S4>}>
% ==

rdf_shared_properties(Graph, AssocN):-
  empty_assoc(Assoc0),
  rdf_subjects(Graph, Subjects),
  findall(
    Subject1-Subject2,
    (
      member(Subject1, Subject2, Subjects),
      Subject1 @< Subject2
    ),
    Pairs
  ),
  foldl(put_shared_properties0(Graph), Pairs, Assoc0, AssocN).

put_shared_properties0(Graph, Subject1-Subject2, OldAssoc, NewAssoc):-
  setoff(
    Property,
    (
      rdf(Subject1, Property, Object, Graph),
      rdf(Subject2, Property, Object, Graph)
    ),
    Properties
  ),
  if_then_else(
    ord_empty(Properties),
    NewAssoc = OldAssoc,
    put_assoc(Properties, OldAssoc, [Subject1, Subject2], NewAssoc)
  ).

