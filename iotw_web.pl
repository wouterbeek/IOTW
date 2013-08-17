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
@version 2013/05, 2013/08
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(meta_ext)).
:- use_module(html(html)).
:- use_module(iotw(iimb)).
:- use_module(iotw(iotw_inodes)).
:- use_module(iotw(iotw_export)).
:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(rdf(rdf_graph)).
:- use_module(server(dev_server)).
:- use_module(server(web_console)).

% First load the debug server.
:- initialization(start_dev_server, now).

:- http_handler(root(identity_node), identity_node, []).

:- register_module(iotw_web).



iimb_web(Integer, SVG):-
  iimb(Integer, SVG).

%! identity_node(+Request:list(nvpair)) is det.
% Callback HTTP handler reaction on a click action on an identity node.

identity_node(Request):-
  member(search(SearchParameters), Request),
  (
    memberchk(id=GAK_Hash, SearchParameters)
  ->
    identity_node(GAK_Hash)
  ;
    true
  ).

node(GAK_Hash):-
  update_identity_node(GAK_Hash),
  export_identity_nodes(GAK_Hash, SVG),
  push(console_output, SVG).

%! pair_to_dom(+Pair:pair(uri), -Markup:list) is det.

pair_to_dom(X-Y, Markup):-
  rdf_po_pairs(X, X_PO_Pairs1), % PredicateObjectPairs
  rdf_po_pairs(Y, Y_PO_Pairs1), % PredicateObjectPairs

  % The table of shared properties.
  select_shared_properties(
    X_PO_Pairs1,
    Y_PO_Pairs1,
    SharedPropertyPairs,
    X_PO_Pairs2,
    Y_PO_Pairs2
  ),
  list_to_table(
    [caption('Table showing the shared properties.'),header(true)],
    [['Predicate','Object']|SharedPropertyPairs],
    SharedPropertyTable
  ),

  % The table of shared predicates.
  select_shared_predicates(
    X_PO_Pairs2,
    Y_PO_Pairs2,
    SharedPredicateTriples,
    X_PO_Pairs3,
    Y_PO_Pairs3
  ),
  list_to_table(
    [caption('Table showing the shared predicates.'),header(true)],
    [['Predicate','X-Object','Y-Object']|SharedPredicateTriples],
    SharedPredicateTable
  ),

  % The table of exclusive X-properties.
  list_to_table(
    [caption('Table showing the exclusive X predicates.'),header(true)],
    [['X-Predicate','X-Object']|X_PO_Pairs3],
    X_Table
  ),

  % The table of exclusive Y-properties.
  list_to_table(
    [caption('Table showing the exclusive Y predicates.'),header(true)],
    [['Y-Predicate','Y-Object']|Y_PO_Pairs3],
    Y_Table
  ),

  Markup =
    [
      element(h1,[],['X: ',X]),
      element(h1,[],['Y: ',Y]),
      SharedPropertyTable,
      SharedPredicateTable,
      X_Table,
      Y_Table
    ].

/*
  identity_node(GAK_Hash,GA_Hash,Key,_,_,_),
  graph_alignment(GA_Hash,G,_,PsAssoc,_,_),
  assoc:get_assoc(Key, PsAssoc, KeyIdentityPairs),
  predicates_to_pairs(G, Key, KeyPairs),
  ord_subtract(KeyPairs, KeyIdentityPairs, KeyNonIdentityPairs),
  maplist(pair_to_dom, KeyNonIdentityPairs, DOMs),
  append(DOMs, DOM),
  push(console_output, DOM).
*/
