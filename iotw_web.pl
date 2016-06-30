:- module(iotw_web, []).

/** <module> IOTW Web

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(html/content/html_collection), except([html_list//1,html_list//2])).
:- use_module(library(html/element/html_link)). % Meta-predicate.
:- use_module(library(html/element/html_list)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_receive)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(tab/tab)). % Load plTabular for browsing.

:- use_module(iotw(inode)).
:- use_module(iotw(iotw_iimb)).

:- http_handler(/, iotw, [id(iotw)]).





% Callback HTTP handler reaction on a click action on an identity node.
iotw(Req):-
  http_query(Req, inode, Hash), !,
  reply_html_page(tab, \iotw_head, \iotw_body(inode(Hash))).
% Show IIMB SVG graphic.
iotw(Req):-
  http_query(Req, iimb, N0), !,
  atom_number(N0, N),
  reply_html_page(tab, \iotw_head, \iotw_body(iimb(N))).
% Normal Web page.
iotw(_):-
  reply_html_page(tab, \iotw_head, \iotw_body(_)).


iotw_body(Content) -->
  {
    findall(
      Location-Label,
      (
        between(1, 80, N),
        http_link_to_id(iotw, [iimb(N)], Location),
        atomic_list_concat([iimb,N], '_', Label)
      ),
      Pairs
    )
  },
  html([
    \iotw_content(Content),
    \html_list(Pairs, [item_writer(html_link),ordered(false)])
  ]).


iotw_content(Var) --> {var(Var)}, !, html([]).
iotw_content(inode(Hash)) --> !,
  {iimb_current(N)},
  html([
    \iotw_content(iimb(N)),
    \iotw_table(Hash)
  ]).
iotw_content(iimb(N)) --> !,
  {iimb_experiment(N, Dom)},
  html(div([id=ihier],\xml_dom_as_atom(Dom))).


iotw_head -->
  html([title('IOTW')]).


iotw_table(Hash) -->
  {
    once(
      inode(
        _,
        Hash,
        IHierHash,
        SharedPs,
        _,
        NumberOfIPairs,
        IPairs,
        NumberOfPairs,
        Pairs
      )
    ),
    once(ihier(G, IHierHash, _, _, _, _)),
    phrase(set(dcg_print_term, SharedPs), Codes),
    atom_codes(SharedLabel, Codes),
    format(
      atom(Description),
      "Enumeration of non-identity pairs sharing ~w (Pairs:~:d;Identity pairs:~:d)",
      [SharedLabel,NumberOfIPairs,NumberOfPairs]
    ),
    ord_subtract(Pairs, IPairs, NonIPairs),
    findall(
      S1-S2-L,
      (
        member(S1-S2, NonIPairs),
        findall([S1,P1,O1], rdf(S1, P1, O1, G), L1),
        findall([S2,P2,O2], rdf(S2, P2, O2, G), L2),
        append(L1, L2, L)
      ),
      Ts
    )
  },
  html([
    p(Description),
    div(id=resources, \generate_triples(Ts))
  ]).


generate_triples([S1-S2-Rows|Ts]) -->
  rdf_html_table(
    Rows,
    [
      caption(html(['Overview of non-identity pair ',\pair(S1,S2),'.'])),
      header_spec(spo),
      indexed(true),
      location(iotw)
    ]
  ),
  generate_triples(Ts).
generate_triples([]) --> html([]).
cell0(X-Y) --> html_pair(X, Y).
