% Debug file for the Identity on the Web (IOTW) project.

:- [load].



% Debug flags.
:- use_module(library(debug)).

:- debug(assoc_ext).
:- debug(inode).
:- debug(inodes_evaluate).
:- debug(iotw).
:- debug(oaei).



% Startup.

:- use_module(iotw(experiments/iotw_iimb)).
:- iimb_experiment(N, Svg),
   maplist(writeln, [N,Svg]).

