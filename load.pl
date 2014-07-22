% The load file for Identity on the Web (IOTW).

:- multifile(user:project/3).
   user:project(
     'IOTW',
     'Identity on the Web. Researching the owl:sameAs relation.',
     iotw
   ).

:- set_prolog_stack(global, limit(2*10**9)).

:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plHtml,
    plRdf,
    plGraphViz,
    plTabular
]).

