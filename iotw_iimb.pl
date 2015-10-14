:- module(
  iotw_iimb,
  [
    iimb_experiment/2 % ?N:between(1,80)
                      % -Dom:list(compound)
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- rdf_register_prefix(
  'IIMB',
  'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
).





%! iimb_experiment(+N:between(0,80), -Svg:dom) is det.
%! iimb_experiment(-N:between(0,80), -Svg:dom) is multi.
% Calculates the identity hierarchy for every IIMB example (80 items).

iimb_experiment(N, Svg):-
  init_iimb(Dir),
  rdf_convert_directory(Dir, ntriples, _, [overwrite(true)]),
  % @tbd OWL materialization right here.
  rdf_convert_directory(Dir, ntriples, _, [overwrite(true)]),
  between(1, 80, N),
  iimb_experiment(Dir, N, Svg).

%! iimb_experiment(+Directory:atom, +Number:between(1,80), Svg:dom) is det.
% Calculates the identity hierarchy for a specific IIMB example.

iimb_experiment(Dir, N, Svg):-
  iimb_experiment_from_files(
    Dir,
    N,
    BaseOntologyFile,
    AlignedOntologyFile,
    ReferenceAlignmentSets
  ),
  atomic_list_concat([iimb,N], '_', Graph),
  rdf_load_any([graph(Graph)], [BaseOntologyFile,AlignedOntologyFile]),
  run_experiment(
    Graph,
    ReferenceAlignmentSets,
    Svg,
    [evaluate(true),granularity(p)]
  ).
/*
iimb_experiment(FromDir, ToDir, N):-
  iimb_experiment_from_files(
    FromDir,
    N,
    BaseOntologyFile,
    AlignedOntologyFile,
    ReferenceAlignments
  ),

  % To file.
  atomic_list_concat([iimb,N], '_', ToFileName),
  absolute_file_name(
    ToFileName,
    ToFileOWL,
    [access(write),file_type(turtle),relative_to(ToDir)]
  ),

  % Execute the goal on the two ontologies.
  rdf_setup_call_cleanup(
    [],
    [BaseOntologyFile,AlignedOntologyFile],
    % Now that all files are properly loaded, we can run the experiment.
    run_experiment(ReferenceAlignments, SvgDom, [evaluate(true),granulaity(p)]),
    [format(turtle)],
    ToFileOWL
  ),
  file_type_alternative(ToFileOWL, svg, ToFileSVG),

  % Remove the file if it already exists.
  safe_delete_file(ToFileSVG),

  % Make sure there is write access.
  access_file(ToFileSVG, write),

  % Write the SVG DOM to file.
  xml_dom_to_file([dtd(svg)], SvgDom, ToFileSVG),

  % STATS
  ap_stage_tick.
*/

%! iimb_experiment_from_files(
%!   +Directory:atom,
%!   +N:between(1,80),
%!   -BaseOntologyFile:atom,
%!   -AlignedOntologyFile:atom,
%!   -ReferenceAlignmentSets:ordset(ordset(iri))
%! ) is det.

iimb_experiment_from_files(
  Dir,
  N,
  BaseOntologyFile,
  AlignedOntologyFile,
  ReferenceAlignmentSets
):-
  format_integer(N, 3, SubDirName),
  absolute_file_name(
    SubDirName,
    SubDir,
    [access(read),file_type(directory),relative_to(Dir)]
  ),

  % The base ontology.
  absolute_file_name(
    onto,
    BaseOntologyFile,
    [access(read),file_type(turtle),relative_to(Dir)]
  ),

  % The aligned ontology.
  absolute_file_name(
    onto,
    AlignedOntologyFile,
    [access(read),file_type(turtle),relative_to(SubDir)]
  ),

  % The reference alignments
  % (between the base ontology and the aligned ontology).
  absolute_file_name(
    refalign,
    ReferenceAlignmentsFile,
    [access(read),file_type(turtle),relative_to(SubDir)]
  ),
  oaei_file_to_alignments(ReferenceAlignmentsFile, ReferenceAlignmentPairs1),
  exclude(
    pair_ext:reflexive_pair,
    ReferenceAlignmentPairs1,
    ReferenceAlignmentPairs2
  ),
  pair_ext:pairs_to_sets(
    ReferenceAlignmentPairs2,
    ReferenceAlignmentSets,
    [reflexive(false),symmetric(false)]
  ).



%! init_iimb(-JoinedPairs:list(pair)) is det.

init_iimb(JoinedPairs):-
  current_prolog_flag(argv, Path),
  directory_file_path(_, 'IIMB.tar.gz', Path), !,
gtrace,
  rdf_download(Url, FromFile, [pairs(Pairs)]),
  findall(
    N-Name,
    (
      member(Graph-_, Pairs),
      http_path_correction(Graph, ToFile),
      relative_file_path(ToFile, FromFile, RelativeFile),
      file_name(RelativeFile, RelativeDir, Name, rdf),
      directory_subdirectories(RelativeDir, SubDirs),
      last(SubDirs, N)
    ),
    Pairs
  ),
  group_pairs_by_key(Pairs, JoinedPairs),
  writeln(JoinedPairs).
