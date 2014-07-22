:- module(
  anatomy,
  [
  ]
).

/** <module> Anatomy

Runs IOTW experiments on the anatomy alignment data.

@author Wouter Beek
@version 2013/05, 2013/08, 2014/07
*/

:- use_module(generics(db_ext)).
:- use_module(iotw(iotw_relatedness)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(os(filepath_ext)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf_file(rdf_serial)).
:- use_module(standards(oaei)).

:- rdf_register_prefix(oboInOwl, 'http://www.geneontology.org/formats/oboInOwl#').
:- rdf_register_prefix(oboRel, 'http://www.obofoundry.org/ro/ro.owl#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).
   user:file_search_path(anatomy, oaei2012('Anatomy')).
   user:file_search_path(raw_results, anatomy('Raw results')).



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
      export_ihier_as_svg(GA_Hash, _SVG, [])
    )
  ).

