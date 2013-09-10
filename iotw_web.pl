:- module(
  iotw_web,
  [
    ipairs_web/1, % -DOM:list
    iimb_web/2 % +Integer:integer
               % -SVG:dom
  ]
).

/** <module> IOTW Web

@author Wouter Beek
@tbd Implement answer to JavaScript callback function.
@version 2013/05, 2013/08-2013/09
*/

:- use_module(generics(meta_ext)).
:- use_module(html(html_table)).
:- use_module(iotw(iimb)).
:- use_module(iotw(inode_export)).
:- use_module(iotw(inode_update)).
:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(rdf(rdf_name)).
:- use_module(server(dev_server)).
:- use_module(server(web_console)).

% First load the debug server.
:- initialization(start_dev_server, now).

:- http_handler(root(inode), inode, []).
:- http_handler(root(resource), resource, []).

:- register_module(iotw_web).



iimb_web(Integer, SVG):-
  iimb([deduction(none),granularity(p)], Integer, SVG).

%! inode(+Request:list(nvpair)) is det.
% Callback HTTP handler reaction on a click action on an identity node.

inode(Request):-
  member(search(SearchParameters), Request),
  (
    memberchk(id=GAK_Hash, SearchParameters)
  ->
    update_identity_node(GAK_Hash),
    export_inodes([], GAK_Hash, SVG, _PDF_File),
    push(console_output, SVG)
  ;
    true
  ).

ipair(pair(X,R,Y,G)):-
  rdf(X, owl:sameAs, Y, G),
  rdf_global_id(owl:sameAs, R).

%! ipairs(
%!   +MaximumNumberOfPairs:nonneg,
%!   -IdentityPairs:list(pair(iri)),
%!   -MorePairsLeft:boolean
%! ) is det.

ipairs(Max, IPairs, Left):-
  ord_empty(Hist),
  ipairs(Max, Hist, IPairs, Left).

ipair_to_row(
  pair(X1,_,Y1,G),
  [element(a,[href=URI2],[X2]),N1,element(a,[href=URI2],[Y2]),N2,G]
):-
  http_absolute_uri(root(resource), URI1),
  uri_components(URI1, Components1),
  uri_query_components(QueryString, [r1=X1,r2=Y1]),
  uri_data(search, Components1, QueryString, Components2),
  uri_components(URI2, Components2),
  rdf_term_name([], X1, X2),
  rdf_term_name([], Y1, Y2),
  resource_rows(X1, Rows1),
  length(Rows1, N1),
  resource_rows(Y1, Rows2),
  length(Rows2, N2).

% Maximum number of pairs reached.
ipairs(0, IPairs, IPairs, Left):-
  boolean(
    (
      ipair(IPair),
      \+ memberchk(IPair, IPairs)
    ),
    Left
  ).
% Another pair is found and can be added
% (the maximum number of pairs is not reached yet).
ipairs(Counter1, Hist1, IPairs, Left):-
  ipair(IPair),
  \+ memberchk(IPair, Hist1),
  ord_add_element(Hist1, IPair, Hist2),
  Counter2 is Counter1 - 1,
  ipairs(Counter2, Hist2, IPairs, Left).
% There are no more pairs.
ipairs(_Counter, IPairs, IPairs, false).

ipairs_web(DOM):-
  ipairs(25, IPairs, _Left),
  length(IPairs, L),
  L > 0, !,
  maplist(ipair_to_row, IPairs, Rows),
  html_table(
    [
      caption('The currently loaded identity pairs.'),
      header(true),
      indexed(true)
    ],
    [['Resource A','#A','Resource B','#B','Triple location']|Rows],
    DOM
  ).
ipairs_web([element(p,[],['There are no identity pairs.'])]).

%! resource(+Request) is det.
% Describes resources (query term `r1`)
% or resource pairs (query terms `r1` and `r2`).

% Describe resource pairs.
resource(Request):-
gtrace,
  option(search(Query), Request),
  option(r1(Resource1), Query),
  option(r2(Resource2), Query), !,
  resource_table(Resource1, Table1),
  resource_table(Resource2, Table2),
  push(console_output, [Table1,Table2]).
% Describe a single resource.
resource(Request):-
  option(search(Query), Request),
  option(r1(Resource), Query), !,
  resource_table(Resource, Table),
  push(console_output, [Table]).

%! resource_rows(+Resource:iri, -Rows:list(atom)) is det.

resource_rows(Resource, Rows):-
  findall(
    [P2,O2],
    (
      rdf(Resource, P1, O1),
      rdf_term_name([], P1, P2),
      rdf_term_name([], O1, O2)
    ),
    Rows
  ).

%! resource_table(+Resource:iri, -Table) is det.

resource_table(Resource, Table):-
  resource_rows(Resource, Rows),
  rdf_term_name([], Resource, ResourceName),
  format(atom(Caption), 'Description of resource ~w.', [ResourceName]),
  Header = ['Predicate','Object'],
  html_table(
    [caption(Caption),header(true),indexed(true)],
    [Header|Rows],
    Table
  ).

/*
pair_to_dom(X-Y, Markup):-
  rdf_po_pairs(X, X_PO_Pairs),
  rdf_po_pairs(Y, Y_PO_Pairs),

  % The table of shared predicates and objects.
  rdf_shared_po_pairs(
    X_PO_Pairs,
    Y_PO_Pairs,
    Shared_PO_Pairs,
    X_Exclusive_PO_Pairs,
    Y_Exclusive_PO_Pairs
  ),
  html_table(
    [caption('Table showing the shared properties.'),header(true)],
    [['Predicate','Object']|Shared_PO_Pairs],
    Shared_PO_Table
  ),

  % The table of shared predicates and different objects.
  rdf_shared_p_triples(
    X_Exclusive_PO_Pairs,
    Y_Exclusive_PO_Pairs,
    Shared_P_Triples,
    X_Exclusive_P_Pairs,
    Y_Exclusive_P_Pairs
  ),
  html_table(
    [caption('Table showing the shared predicates.'),header(true)],
    [['Predicate','X-Object','Y-Object']|Shared_P_Triples],
    Shared_P_Table
  ),

  % The table of exclusive X-properties.
  html_table(
    [caption('Table showing the exclusive X predicates.'),header(true)],
    [['X-Predicate','X-Object']|X_Exclusive_P_Pairs],
    X_Exclusive_P_Table
  ),

  % The table of exclusive Y-properties.
  html_table(
    [caption('Table showing the exclusive Y predicates.'),header(true)],
    [['Y-Predicate','Y-Object']|Y_Exclusive_P_Pairs],
    Y_Exclusive_P_Table
  ),

  Markup =
    [
      element(h1,[],['X: ',X]),
      element(h1,[],['Y: ',Y]),
      Shared_PO_Table,
      Shared_P_Table,
      X_Exclusive_P_Table,
      Y_Exclusive_P_Table
    ].
*/

