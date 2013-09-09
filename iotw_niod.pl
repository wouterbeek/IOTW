:- module(
  iotw_niod,
  [
    iotw_niod/0
  ]
).

/** <module> IOTW experiment with the Verenigd Koninkrijk dataset.

@author Wouter Beek
@version 2013/09
*/

:- use_module(generics(db_ext)).
:- use_module(library(debug)).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(xpath)).
:- use_module(vocabularies(void)).
:- use_module(xml(xml_namespace)).

:- db_add_novel(user:file_search_path(vk_data, data('VK'))).
:- db_add_novel(user:file_search_path(vk_rdf, vk_data('RDF'))).
:- db_add_novel(user:file_search_path(vk_dbpedia, vk_rdf(dbpedia))).

:- db_add_novel(user:prolog_file_type(ttl, turtle)).

:- rdf_meta(rdf_literal(r,r,?)).
:- rdf_meta(rdf_literal(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?)).
:- rdf_meta(top_messages(+, r)).

:- xml_register_namespace('category-nl', 'http://nl.dbpedia.org/resource/').
:- xml_register_namespace('prop-nl', 'http://nl.dbpedia.org/property/').
:- xml_register_namespace(dbpedia, 'http://dbpedia.org/resource/').
:- xml_register_namespace('dbpedia-owl', 'http://dbpedia.org/ontology/').
:- xml_register_namespace(niod, 'http://purl.org/collections/nl/niod/').
:- xml_register_namespace(schema, 'http://schema.org/').
:- xml_register_namespace(vk, 'http://www.wouterbeek.com/vk.owl#').

:- debug(iotw_niod).



iotw_niod:-
  absolute_file_name(
    vk_rdf(void),
    VoID_File,
    [access(read),file_type(turtle)]
  ),
  void_load_library(VoID_File, _, VoID_Graph),
  debug(iotw_niod, 'VoID graph ~w is loaded.', [VoID_Graph]).
/*
  rdf_attach_library(File),

  rdf_load_library(viervijfmeidata, []),
  rdf_load_library('niod-bb2', []),
  rdf_load_library('niod-vk-botb', []),
  rdf_load_library('niod-vk-entities', []),
  rdf_load_library('niod-vk-pillars', []),

  absolute_file_name(
    vk_dbpedia(dbpedia_dump),
    DBpediaFile,
    [access(read), file_type(turtle)]
  ),
  rdf_load(
    DBpediaFile,
    [
      cache(true),
      format(turtle),
      graph(dbpedia),
      if(not_loaded),
      register_namespaces(true),
      silent(fail) % We like to get feedback.
    ]
  ).
*/

