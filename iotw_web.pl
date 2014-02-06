:- module(iotw_web, []).

/** <module> IOTW Web

@author Wouter Beek
@tbd Implement answer to JavaScript callback function.
@version 2013/05, 2013/08-2013/09, 2013/11-2014/01
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generics)).
:- use_module(generics(db_ext)).
:- use_module(generics(uri_ext)).
:- use_module(html(html)). % Requires the DTD file location for HTML.
:- use_module(html(html_table)).
:- use_module(iotw(inode)).
:- use_module(iotw_exp(iotw_iimb)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_name)).
:- use_module(server(web_modules)).
:- use_module(xml(xml_dom)).

:- http_handler(root(iotw), iotw, []).
:- initialization(web_module_add('IOTW', iotw_web)).

% /js
:- db_add_novel(http:location(js, root(js), [])).
:- db_add_novel(user:file_search_path(js, iotw(js))).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).
:- html_resource(js('iotw.js'), []).

:- dynamic(iimb_current/1).
:- dynamic(max/4).



% Callback HTTP handler reaction on a click action on an identity node.
iotw(Request):-
  memberchk(search(Search), Request),
  memberchk(inode=GAK_Hash, Search), !,
  reply_html_page(
    app_style,
    \iotw_head,
    \iotw_body(inode(GAK_Hash))
  ).
% Show IIMB SVG graphic.
iotw(Request):-
  memberchk(search(Search), Request),
  memberchk(iimb=N, Search), !,
  reply_html_page(app_style, \iotw_head, \iotw_body(iimb(N))).
% Normal Web page.
iotw(_Request):-
  reply_html_page(app_style, \iotw_head, \iotw_body(_)).

iotw_body(Content) -->
  {
    findall(
      element(li,[],[element(a,[href=IOTW_URL2],[Name])]),
      (
        between(1, 80, N),
        http_absolute_uri(root(iotw), IOTW_URL1),
        uri_query_add(IOTW_URL1, iimb, N, IOTW_URL2),
        atomic_list_concat([iimb,N], '_', Name)
      ),
      HTML_DOM
    )
  },
  html([\iotw_content(Content),div(id=index,ol([],HTML_DOM))]).

iotw_content(Var) -->
  {var(Var)}, !.
iotw_content(inode(GAK_Hash)) --> !,
  {iimb_current(N)},
  html([
    \iotw_content(iimb(N)),
    \iotw_table(GAK_Hash)
  ]).
iotw_content(iimb(N)) --> !,
  {
    db_replace_novel(iimb_current(N), [r]),
    iimb_experiment(N, SVG_DOM),
    xml_dom_to_atom([], SVG_DOM, SVG_Atom)
  },
  html(div([id=ihier],\[SVG_Atom])).

iotw_head -->
  html([
    \html_requires(js('iotw.js')),
    title('IOTW')
  ]).

iotw_table(GAK_Hash) -->
  {
    once(
      inode(
        _Mode,
        GAK_Hash,
        IHierHash,
        SharedPs,
        _Approx,
        NumberOfIPairs,
        IPairs,
        NumberOfPairs,
        Pairs
      )
    ),
    once(ihier(IHierHash, G, _, _, _, _)),
    phrase(set([], rdf_term_name, SharedPs), Codes),
    atom_codes(SharedLabel, Codes),
    format(
      atom(Description),
      'Enumeration of non-identity pairs sharing ~w (Pairs:~d;Identity pairs:~d)',
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
        append([[['Subject','Predicate','Object']],L1,L2], L)
      ),
      Triples
    )
  },
  html([
    p(Description),
    div(id=resources, \generate_triples(Triples))
  ]).

generate_triples([S1-S2-Rows|Triples]) -->
  html_table(
    [header_row(true),indexed(true)],
    \caption(S1, S2),
    html_pl_term,
    Rows
  ),
  generate_triples(Triples).
generate_triples([]) --> [].

caption(S1, S2) -->
  html([
    'Overview of non-identity pair ',
    \pair([], pl_term, [S1,S2]),
    '.'
  ]).
