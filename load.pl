project_name('Identity on the Web').

load_iotw:-
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),
  
  source_file(load_iotw, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  
  % Use a subdirectory for data files.
  assert(user:file_search_path(data, project('Data'))),
  assert(user:file_search_path(oaei2012, data('OAEI 2012'))),
  assert(user:file_search_path(anatomy, oaei2012('Anatomy'))),
  assert(user:file_search_path(anatomy_raw, anatomy('Raw results'))),
  
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
  ensure_loaded(project(iotw)).

:- load_iotw.

