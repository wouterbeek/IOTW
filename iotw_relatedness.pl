:- module(
  iotw_relatedness,
  [
    rdf_shared_properties/1 % +Graph:atom
  ]
).

/** <module>

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(assoc_multi)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).



%! rdf_shared_properties(+Graph:atom) is det.
% Loads the pairs that share the same properties into an association list
% with ordsets.
%
% # Example
%
% Given the following 4 triples:
% ==
% <S1, P, O12>
% <S2, P, O12>
% <S3, P, O34>
% <S4, P, O34>
% ==
%
% The association list would be:
% ==
% <{P},{<S1,S2>,<S3,S4>}>
% ==

rdf_shared_properties(Graph):-
  AssocName = Graph,
  register_assoc(AssocName),
  forall(
    (
      rdf_subject(Graph, Subject1),
      rdf_subject(Graph, Subject2),
      Subject1 \== Subject2
    ),
    (
      setoff(
        Property,
        (
          rdf(Subject1, Property, Object, Graph),
          rdf(Subject2, Property, Object, Graph)
        ),
        Properties
      ),
      put_assoc(Properties, AssocName, [Subject1, Subject2])
    )
  ).

