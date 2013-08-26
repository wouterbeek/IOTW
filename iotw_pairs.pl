:- module(
  iotw_pairs,
  [
    lattice/1, % +Graph:atom
    lattice_export/1 % -GIF:compound
  ]
).

/** <module> IOTW_PAIRS

Classifies *all* resource pairs by the predicates they share.

@author Wouter Beek
@version 2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_name)).
:- use_module(rdf(rdf_term)).

:- dynamic edge/2, node/4.

:- debug(iotw_pairs).



lattice(G):-
  % Setup.
  rdf_graph(G),
  flag(column, _, 1),
  retractall(edge(_,_)),
  retractall(node(_,_,_,_)),

  setoff(P-O, rdf(_, P, O, G), POs),

  debug(iotw_pairs, 'Test', []),
  % Lattice row 1.
  forall(
    member(P-O, POs),
    (
      setoff(S, rdf(S, P, O, G), Ss),
      flag(column, Id, Id + 1),
      assert(node(1, Id, [P-O], Ss))
    )
  ),
  flag(column, _, 1),

  % Lattice row N+1 based on lattice row N.
  length(POs, L),
  forall(
    between(2, L, N),
    (
      lattice_row(L, N),
      flag(column, _, 1)
    )
  ).

lattice_row(L, N):-
  succ(PreviousN, N),
  binomial_coefficient(L, N, Max),
  binomial_coefficient(L, PreviousN, BC),
  succ(BCmin, BC),
  between(1, BCmin, I),
  between(2, BC, J),
  lattice_cell(Max, PreviousN, N, I, J), !.
lattice_row(_L, _N).

lattice_cell(Max, _PreviousN, _N, _I, _J):-
  flag(it, Max, Max), !, fail.
lattice_cell(_Max, PreviousN, N, I, J):-
  node(PreviousN, I, POs1, Ss1),
  node(PreviousN, J, POs2, Ss2),
  ord_union(POs1, POs2, POs),
  ord_intersection(Ss1, Ss2, Ss),
  debug(iotw_pairs, '~w CUP ~w\t~wCAP~w', [POs1,POs2,Ss1,Ss2]),
  flag(column, Id, Id + 1),
  assert(node(N, Id, POs, Ss)),
  assert(edge(node(PreviousN,I,POs1,Ss1),node(N,Id,POs,Ss))),
  assert(edge(node(PreviousN,J,POs2,Ss2),node(N,Id,POs,Ss))), !.
% No I and/or no J? That's fine!
lattice_cell(_Max, _PreviousN, _N, _I, _J).

lattice_export(GIF):-
  setoff(
    Row,
    node(Row, _, _, _),
    Rows
  ),
  findall(
    rank(vertex(RankId,RankId,RankAttrs),V_Terms),
    (
      member(Row,Rows),
      format(atom(RankId), 'r~w', [Row]),
      atom_number(RankLabel, Row),
      RankAttrs = [label(RankLabel), shape(plaintext)],

      findall(
        vertex(V_Name,node(Row,Col,POs,Ss),V_Attrs),
        (
          node(Row, Col, POs, Ss),
          Ss \== [],
          vertex_id(Row, Col, V_Name),
          rdf_name:rdf_term_pairs_name([], POs, PsLabel),
          length(Ss, Size),
          format(
            atom(V_Label),
            '~w\n~w triples',
            [PsLabel,Size]
          ),
          V_Attrs =
            [color(darkgreen),label(V_Label),shape(rectangle),style(solid)]
        ),
        V_Terms
      )
    ),
    Ranks
  ),

  E_Attrs = [color(black),style(solid)],
  findall(
    edge(FromId,ToId,E_Attrs),
    (
      edge(node(Row1,Col1,_,Ss1),node(Row2,Col2,_,Ss2)),
      Ss1 \== [],
      Ss2 \== [],
      vertex_id(Row1, Col1, FromId),
      vertex_id(Row2, Col2, ToId)
    ),
    E_Terms
  ),

  G_Attrs =
    [colorscheme(svg),charset('UTF-8'),fontsize(11.0),overlap(false)],

  GIF = graph([], Ranks, E_Terms, [G_Attrs]).

vertex_id(Row, Col, Id):-
  format(atom(Id), 'r~wc~w', [Row,Col]).

