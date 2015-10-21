:- module(
  iotw_web,
  [
    iotw/2 % +Request:list(nvpair)
           % +HtmlStype:compound
  ]
).

/** <module> IOTW Web

@author Wouter Beek
@tbd Implement answer to JavaScript callback function.
@version 2013/05, 2013/08-2013/09, 2013/11-2014/01, 2014/03-2014/04, 2014/07
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_collection)).
:- use_module(generics(db_ext)).
:- use_module(generics(uri_search)).
:- use_module(xml(xml_dom)).

:- use_module(plRdf(rdf_name)). % DCG-meta.

:- use_module(plHtml(html)).
:- use_module(plHtml(html_list)).

:- use_module(plTabular(rdf_html_table)).

:- use_module(iotw(inode)).
:- use_module(iotw_exp(iotw_iimb)).

:- html_resource(js('iotw.js'), []).

:- dynamic(iimb_current/1).



%! iotw(+Request:list(nvpair), +HtmlStyle:atom) is det.

% Callback HTTP handler reaction on a click action on an identity node.
iotw(Request, HtmlStyle):-
  request_search_read(Request, inode, GakHash), !,
  reply_html_page(
    HtmlStyle,
    \iotw_head,
    \iotw_body(inode(GakHash))
  ).
% Show IIMB SVG graphic.
iotw(Request, HtmlStyle):-
  request_search_read(Request, iimb, N1), !,
  atom_number(N1, N2),
  reply_html_page(HtmlStyle, \iotw_head, \iotw_body(iimb(N2))).
% Normal Web page.
iotw(_, HtmlStyle):-
  reply_html_page(HtmlStyle, \iotw_head, \iotw_body(_)).

iotw_body(Content) -->
  {
    findall(
      Location2-Label,
      (
        between(1, 80, N),
        http_absolute_uri(iotw(home), Location1),
        uri_search_add(Location1, iimb, N, Location2),
        atomic_list_concat([iimb,N], '_', Label)
      ),
      Pairs
    )
  },
  html([
    \iotw_content(Content),
    \html_list(Pairs, html_link, [ordered(false)])
  ]).

iotw_content(Var) --> {var(Var)}, !, [].
iotw_content(inode(GakHash)) --> !,
  {iimb_current(N)},
  html([
    \iotw_content(iimb(N)),
    \iotw_table(GakHash)
  ]).
iotw_content(iimb(N)) --> !,
  {
    db_replace(iimb_current(N), [r]),
    iimb_experiment(N, SvgDom)
  },
  html(div([id=ihier],\xml_dom_as_atom(SvgDom))).

iotw_head -->
  html([
    \html_requires(js('iotw.js')),
    title('IOTW')
  ]).

iotw_table(GakHash) -->
  {
    once(
      inode(
        _Mode,
        GakHash,
        IHierHash,
        SharedPs,
        _Approx,
        NumberOfIPairs,
        IPairs,
        NumberOfPairs,
        Pairs
      )
    ),
    once(ihier(G, IHierHash, _, _, _, _)),
    phrase(set(rdf_term_name, SharedPs), Codes),
    atom_codes(SharedLabel, Codes),
    format(
      atom(Description),
      'Enumeration of non-identity pairs sharing ~w (Pairs:~:d;Identity pairs:~:d)',
      [SharedLabel,NumberOfIPairs,NumberOfPairs]
    ),
    ord_subtract(Pairs, IPairs, NonIPairs),
    findall(
      S1-S2-L,
      (
        member(S1-S2, NonIPairs),
        findall(
          [S1,P1,O1],
          rdf(S1, P1, O1, G),
          L1
        ),
        findall(
          [S2,P2,O2],
          rdf(S2, P2, O2, G),
          L2
        ),
        append(L1, L2, L)
      ),
      Triples
    )
  },
  html([
    p(Description),
    div(id=resources, \generate_triples(Triples))
  ]).

generate_triples([S1-S2-Rows|Triples]) -->
  rdf_html_table(
    [header_row(spo),indexed(true),location(iotw)],
    html([
      'Overview of non-identity pair ',
      \html_pair(rdf_term_html(iotw, S1, S2)),
      '.'
    ]),
    Rows
  ),
  generate_triples(Triples).
generate_triples([]) --> [].

