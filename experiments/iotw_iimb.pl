:- module(
  iotw_iimb,
  [
    iimb_experiment/0,
    iimb_experiment/2 % +Number:between(1,80)
                      % -SvgDom:list
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09, 2013/11-2014/01, 2014/03, 2014/07
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(xml(xml_dom)).

:- use_module(plRdf(oaei)).
:- use_module(plRdf(rdf_meta)).
:- use_module(plRdf_ser(rdf_serial)).

:- use_module(iotw(iotw)).

:- rdf_register_prefix(
  'IIMB',
  'http://oaei.ontologymatching.org/2012/IIMBTBOX/'
).

% DTD used for storing SVG DOM to files.
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).
   user:file_search_path(dtd, svg(.)).



%! iimb_experiment is det.
% Calculates the identity hierarchy for every IIMB example (80 items).

iimb_experiment:-
  % Unpack the archive containing the original OAEI2012 IIMB data.
  absolute_file_name(
    data('IIMB'),
    File,
    [access(read),file_extensions(['tar.gz'])]
  ),
  archive_extract(File, Dir, _Filters, _EntryProperties),
  rdf_convert_directory(Dir, ntriples, _, [overwrite(true)]),
  owl_materialize,

  rdf_convert_directory(Dir, ntriples, _, [overwrite(true)]),
  
  forall(
    between(1, 80, N),
    iimb_experiment(Dir, N, Svg)
  ).

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
  oaei_file_to_alignments(ReferenceAlignmentsFile, ReferenceAlignmentPairs),
  pairs_to_sets(ReferenceAlignmentPairs, ReferenceAlignmentSets).

