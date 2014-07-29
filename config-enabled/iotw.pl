:- module(conf_iotw, []).

:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).

:- if(\+ current_module(load_project)).
  :- if(current_prolog_flag(argv, ['--debug'])).
    :- ensure_loaded('../debug').
  :- else.
    :- ensure_loaded('../load').
  :- endif.
:- endif.

:- use_module(cliopatria(hooks)).



% IOTW: infrastructure.

:- multifile(http:location/3).
   http:location(iotw, cliopatria(iotw), []).

:- multifile(user:file_search_path/2).
   user:file_search_path(js, iotw_web(js)).


% IOTW: home page.

:- use_module(iotw_web(iotw_web)).

cliopatria:menu_item(600=places/iotw, 'IOTW').

:- http_handler(iotw(home), iotw, [id(iotw)]).

iotw(Request):-
  iotw(Request, cliopatria(default)).

