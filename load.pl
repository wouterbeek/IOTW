% The load file for Identity on the Web (IOTW).

:- multifile(user:project/3).
   user:project(
     'IOTW',
     'Identity on the Web. Researching the owl:sameAs relation.',
     iotw
   ).


% Set hardware resources.
:- set_prolog_stack(global, limit(2*10**9)).


% Load submodule projects.
:- use_module(load_project).
:- load_project([
    plc-'Prolog-Library-Collection',
    plHtml,
    plRdf,
    plGraphViz,
    plTabular
]).


% Load SWI-Prolog packages.
:- use_module(pl(pl_package)).
:- load_pl_package(lambda).

