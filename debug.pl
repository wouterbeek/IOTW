:- use_module(library(debug)).

:- debug(assoc_ext).
:- debug(inode).
:- debug(inodes_evaluate).
:- debug(iotw).
:- debug(oaei).

:- [load].

:- use_module(library(apply)).

:- use_module(iotw_iimb).

:- iimb_experiment(N, Svg),
   maplist(writeln, [N,Svg]).
