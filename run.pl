run:-
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),
  
  source_file(run, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  
  % Use a subdirectory for data files.
  assert(user:file_search_path(data, project('Data'))),
  
  % Load the PGC.
  assert(user:file_search_path(pgc, project('PGC'))),
  ensure_loaded(pgc(load)),
  
  % Identity on the Web.
  ensure_loaded(project(identity)).

:- run.

