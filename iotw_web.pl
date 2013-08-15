:- module(
  iotw_web,
  [
    load_iimb_web/2 % +Integer:integer
                    % -SVG:dom
  ]
).

/** <module> IOTW Web

@author Wouter Beek
@version 2013/05, 2013/08
*/

:- use_module(generics(meta_ext)).
:- use_module(gv(gv_hash)).
:- use_module(iotw(iimb)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
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
    member(id=Id, SearchParameters),
    node_(Id)
  ).

node_(Id1):-
  % Use the hashed value to find the assoc key name.
  sub_atom(Id1, 6, _Length, 0, Id2),
  indexed_sha_hash(Key, Id2),

  iotw_relatedness:current_assoc(Assoc),
  assoc:get_assoc(Key, Assoc, TheseIdentityPairs),
  iotw_relatedness:current_graph(Graph),
  iotw_relatedness:predicates_to_pairs(Graph, Key, ThesePairs),
  ord_subtract(ThesePairs, TheseIdentityPairs, TheseNonIdentityPairs),
  findall(
    DOM,
    (
      member(TheseNonIdentityPair, TheseNonIdentityPairs),
      iotw_relatedness:pair_to_dom(TheseNonIdentityPair, DOM)
    ),
    DOMs
  ),
  append(DOMs, DOM),
  push(console_output, DOM).

