:- module(
  iotw_web,
  [
    iimb_web/2 % +Integer:integer
               % -SVG:dom
  ]
).

/** <module> IOTW Web

@author Wouter Beek
@tbd Implement answer to JavaScript callback function.
@version 2013/05, 2013/08-2013/09
*/

:- use_module(html(html)).
:- use_module(iotw(iimb)).
:- use_module(iotw(inode)).
:- use_module(iotw(inode_export)).
:- use_module(iotw(inode_update)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(rdf(rdf_term)).
:- use_module(server(dev_server)).
:- use_module(server(web_console)).

% First load the debug server.
:- initialization(start_dev_server, now).

:- http_handler(root(inode), inode, []).

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
    inode(GAK_Hash)
  ;
    true
  ).

node(GAK_Hash):-
  update_identity_node(GAK_Hash),
  export_identity_nodes(GAK_Hash, SVG, _PDF_File),
  push(console_output, SVG).

%! pair_to_dom(+Pair:pair(uri), -Markup:list) is det.

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
  list_to_table(
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
  list_to_table(
    [caption('Table showing the shared predicates.'),header(true)],
    [['Predicate','X-Object','Y-Object']|Shared_P_Triples],
    Shared_P_Table
  ),

  % The table of exclusive X-properties.
  list_to_table(
    [caption('Table showing the exclusive X predicates.'),header(true)],
    [['X-Predicate','X-Object']|X_Exclusive_P_Pairs],
    X_Exclusive_P_Table
  ),

  % The table of exclusive Y-properties.
  list_to_table(
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

