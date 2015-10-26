:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server)).
:- use_module(library(profile/profile_rdf)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, resource/css).
user:file_search_path(js, resource/js).

init_iotw:-
  source_file(init_iotw, File),
  file_directory_name(File, Dir),
  assert(user:file_search_path(iotw, Dir)),
  start_server.
:- init_iotw.

:- use_module(iotw(iotw_iimb)).

:- iimb_experiment(1, _).
