:- module(
  iotw_iimb,
  [
    iotw_iimb/3 % +Options:list(nvpair)
                % +Number:nonneg
                % -SVG:dom
  ]
).

/** <module> IIMB

Runs IOTW experiments on the IIMB alignment data.

@author Wouter Beek
@version 2013/05, 2013/08-2013/09, 2013/11
*/

:- use_module(ap(ap)).
:- use_module(ap(ap_act)). % Used in ap/2.
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(iotw(iotw)).
:- use_module(os(dir_ext)).
:- use_module(rdf(rdf_ap)).
:- use_module(standards(oaei)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace('IIMBTBOX', 'http://oaei.ontologymatching.org/2012/IIMBTBOX/').

:- db_add_novel(user:prolog_file_type('tar.gz', archive)).
:- db_add_novel(user:prolog_file_type(owl, owl)).



%! iotw_iimb(+Options:list(nvpair), +Number:between(1,80), -SVG:list) is det.
% Loads a specific IIMB alignment into memory and exports the IOTW results.
%
% @param Options
% @param Number The number of the IIMB dataset that is used.
% @param SVG A list of compound terms describing an SVG DOM.

iotw_iimb(O1, N, SVG):-
gtrace,
  ap(
    [process(iimb),project(iotw)],
    [
      ap_stage([from(input,'OAEI2012',archive)], ap_extract_archive),
      ap_stage([args(O1, N,SVG),to(_,ontology,owl)], iimb_experiment)
    ]
  ).

iimb_experiment(_StageAlias, FromDir, ToFile, O1, N, SVG):-
  % Main directory.
  subdirectories_to_directory(
    [FromDir,'OAEI2012','Instance matching','IIMB'],
    IIMB_Dir
  ),

  % The base ontology.
  absolute_file_name(
    onto,
    BaseFile,
    [access(read),file_type(owl),relative_to(IIMB_Dir)]
  ),

  % The aligned ontology.
  format_integer(N, 3, SubDirName),
  absolute_file_name(
    SubDirName,
    SubDir,
    [access(read),file_type(directory),relative_to(IIMB_Dir)]
  ),
  absolute_file_name(
    onto,
    AlignedOntologyFile,
    [access(read),file_type(owl),relative_to(SubDir)]
  ),

  % The reference alignments
  % (between the base ontology and the aligned ontology).
  absolute_file_name(
    refalign,
    A_File,
    [access(read),file_type(rdf),relative_to(SubDir)]
  ),
  oaei_file_to_alignments(A_File, A_Pairs),

  % Execute the goal on the two ontologies.
  rdf_setup_call_cleanup(
    [to(ToFile)],
    % Now that all files are properly loaded, we can run the experiment.
    run_experiment(O1, A_Pairs, SVG),
    [BaseFile,AlignedOntologyFile]
  ).

