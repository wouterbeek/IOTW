:- use_module(library(debug)).

:- debug(assoc_ext).
:- debug(inode).
:- debug(inodes_evaluate).
:- debug(iotw).
:- debug(oaei).

:- [load].

:- use_module(iotw(experiments/iotw_iimb)).
:- iimb_experiment(N, Svg),
   maplist(writeln, [N,Svg]).
