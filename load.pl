% The load file for the Identity on the Web (IOTW) project.

project_name('IOTW').

% Set the global and local stacks to 2GB.
% This requires a 64-bit machine and OS.
:- if((current_prolog_flag(address_bits, X), X >= 64)).
:- set_prolog_stack(global, limit(2*10**9)).
:- set_prolog_stack(local, limit(2*10**9)).
:- endif.

:- initialization(load_iotw).

load_iotw:-
  source_file(load_iotw, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(iotw, ThisDirectory)),

  % Use a subdirectory for data files.
  assert(user:file_search_path(data, project('Data'))),
  assert(user:file_search_path(oaei2012, data('OAEI 2012'))),

  % Load the PGC.
  assert(user:file_search_path(pgc, project('PGC'))),
  (
    predicate_property(debug_project, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),

  % Identity on the Web.
  use_module(project(iotw_web)).

