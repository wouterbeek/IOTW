project_name('IOTW').

load_iotw:-
  source_file(load_iotw, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),

  % Use a subdirectory for data files.
  assert(user:file_search_path(data, project('Data'))),
  assert(user:file_search_path(oaei2012, data('OAEI 2012'))),

  % Load the PGC.
  assert(user:file_search_path(pgc, project('PGC'))),
  (
    predicate_property(debug, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),
  
  % Identity on the Web.
  ensure_loaded(project(iotw)),
  use_module(project(iotw_web)).
:- load_iotw.

