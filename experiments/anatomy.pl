:- module(anatomy, []).

/** <module> Anatomy experiment

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(lists)).

:- use_module(iotw(iotw_relatedness)).



load_alignment_anatomy:-
  % Create a unique graph name.
  rdf_new_graph(anatomy, OntologyGraph),
  
  % Load the human ontology.
  absolute_file_name(
    anatomy(human),
    HumanFile,
    [access(read), file_type(owl)]
  ),
  rdf_load_any([graph(human)], HumanFile),
  
  % Load the mouse ontology.
  absolute_file_name(
    anatomy(mouse),
    MouseFile,
    [access(read), file_type(owl)]
  ),
  rdf_load_any([graph(mouse)], MouseFile),
  rdf_graph_merge([human,mouse], OntologyGraph),
  
  % Process the various alignments.
  absolute_file_name(
    raw_results(.),
    AlignmentDir,
    [access(read), file_type(directory)]
  ),
  path_walk_tree(AlignmentDir, '.*.rdf$', AlignmentFiles),
  forall(
    member(AlignmentFile, AlignmentFiles),
    (
      oaei_file_to_alignment_pairs(AlignmentFile, Alignments),
      create_ihier(OntologyGraph, Alignments, GA_Hash, []),
      export_ihier(GA_Hash, _, [])
    )
  ).
