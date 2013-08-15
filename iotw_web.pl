:- module(
  iotw_web,
  [
    load_iimb_web/2 % +Integer:integer
                    % -SVG:dom
  ]
).

/** <module> IOTW Web

@author Wouter Beek
@tbd Implement answer to JavaScript callback function.
@version 2013/05, 2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(gv(gv_hash)).
:- use_module(html(html)).
:- use_module(iotw(iimb)).
:- use_module(iotw(iotw_alignments_export)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(rdf(rdf_graph)).
:- use_module(server(dev_server)).
:- use_module(server(web_console)).

http:location(root, '/prasem/', []).

:- http_handler(root(node), node, []).

:- register_module(iotw_web).



load_iimb_web(Integer, SVG):-
  load_alignment_iimb(Integer, SVG).

%! node(+Request:list(nvpair)) is det.
% Callback HTTP handler reaction on a click action.
%
% @tbd

node(Request):-
  member(search(SearchParameters), Request),
  if_then(
    (
      memberchk(graph=Graph, SearchParameters),
      memberchk(assoc=AssocName, SearchParameters),
      memberchk(id=Id, SearchParameters),
      assoc_by_name(AssocName, Assoc)
    ),
    node_(Graph, Assoc, Id)
  ).

node_(Graph, Assoc, Id1):-
  % Use the hashed value to find the assoc key name.
  sub_atom(Id1, 6, _Length, 0, Id2),
  indexed_sha_hash(Key, Id2),

  assoc:get_assoc(Key, Assoc, TheseIdentityPairs),
  predicates_to_pairs(Graph, Key, ThesePairs),
  ord_subtract(ThesePairs, TheseIdentityPairs, TheseNonIdentityPairs),
  findall(
    DOM,
    (
      member(TheseNonIdentityPair, TheseNonIdentityPairs),
      pair_to_dom(TheseNonIdentityPair, DOM)
    ),
    DOMs
  ),
  append(DOMs, DOM),
  push(console_output, DOM).

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
