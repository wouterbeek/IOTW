:- set_prolog_stack(global, limit(2*10**9)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server)).

:- start_server.

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, resource/css).
user:file_search_path(js, resource/js).
