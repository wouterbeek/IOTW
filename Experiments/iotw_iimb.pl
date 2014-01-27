:- module(
  iotw_iimb,
  [
    iimb_experiment/0,
    iimb_experiment/2 % +Number:between(1,80)
                      % -SVG_DOM:list
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09, 2013/11-2013/12
*/

:- use_module(ap(ap)).
:- use_module(ap(ap_stat)).
:- use_module(generics(archive_ext)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(iotw(iotw)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(os(dir_ext)).
:- use_module(os(file_ext)).
:- use_module(os(run_ext)).
:- use_module(owl(owl_build)).
:- use_module(owl(owl_mat)).
:- use_module(rdf(rdf_meta)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_serial_conv)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_dom)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('IIMB', 'http://oaei.ontologymatching.org/2012/IIMBTBOX/').

% DTD used for storing SVG DOM to files.
:- db_add_novel(user:file_search_path(dtd, svg(.))).



%! iimb_experiment is det.
% Calculates the identity hierarchy for every IIMB example (80 items).

iimb_experiment:-
  ap(
    [process(iimb),project(iotw)],
    [
      % Unpack the archive containing the original OAEI2012 data.
      ap_stage([from(input,'IIMB',archive)], extract_archive),

      % Make sure all RDF data is stored in the Turtle serialization format.
      ap_stage([args([turtle])], rdf_convert_directory),

      % A Java Maven project does the OWL materialization (using Jena).
      ap_stage([], owl_materialize),

      % Although this step is not strictly needed,
      % it does allow the materialized results to be easily
      % compared on a per-file level
      % (e.g. the comment counting the number of serialized triples).
      ap_stage([args([turtle])], rdf_convert_directory),

      % Run the IOTW experiment.
      ap_stage([between(1,80),to(output)], iimb_experiment)
    ]
  ).

%! iimb_experiment(+Number:between(1,80), SVG_DOM:list) is det.
% Calculates the identity hierarchy for a specific IIMB example.

iimb_experiment(N, SVG_DOM):-
  absolute_file_name(
    iotw(ap/iimb/stage4),
    FromDir,
    [access(read),file_type(directory)]
  ),
  iimb_experiment_from_files(FromDir, N, O_File1, O_File2, A_Pairs),
  atomic_list_concat([iimb,N], '_', G),
  rdf_loads([O_File1,O_File2], G),
  run_experiment([evaluate(true),granularity(p)], A_Pairs, SVG_DOM, G).

iimb_experiment(FromDir, ToDir, N):-
  iimb_experiment_from_files(FromDir, N, O_File1, O_File2, A_Pairs),

  % To file.
  atomic_list_concat([iimb,N], '_', ToFileName),
  absolute_file_name(
    ToFileName,
    ToFileOWL,
    [access(write),file_type(turtle),relative_to(ToDir)]
  ),

  % Execute the goal on the two ontologies.
  rdf_setup_call_cleanup(
    [to(ToFileOWL)],
    % Now that all files are properly loaded, we can run the experiment.
    run_experiment([evaluate(true),granulaity(p)], A_Pairs, SVG_DOM),
    [O_File1,O_File2]
  ),
  file_type_alternative(ToFileOWL, svg, ToFileSVG),

  % Remove the file if it already exists.
  safe_delete_file(ToFileSVG),

  % Make sure there is write access.
  access_file(ToFileSVG, write),

  % Write the SVG DOM to file.
  xml_dom_to_file([dtd(svg)], SVG_DOM, ToFileSVG),

  % STATS
  ap_stage_tick.

iimb_experiment_from_files(FromDir, N, O_File1, O_File2, A_Pairs):-
  format_integer(N, 3, SubDirName),
  absolute_file_name(
    SubDirName,
    SubDir,
    [access(read),file_type(directory),relative_to(FromDir)]
  ),

  % The base ontology.
  absolute_file_name(
    onto,
    O_File1,
    [access(read),file_type(turtle),relative_to(FromDir)]
  ),

  % The aligned ontology.
  absolute_file_name(
    onto,
    O_File2,
    [access(read),file_type(turtle),relative_to(SubDir)]
  ),

  % The reference alignments
  % (between the base ontology and the aligned ontology).
  absolute_file_name(
    refalign,
    A_File,
    [access(read),file_type(turtle),relative_to(SubDir)]
  ),
  oaei_file_to_alignments(A_File, A_Pairs).

