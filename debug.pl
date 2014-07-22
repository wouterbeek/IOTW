% Debug file for the DataHives project.

:- [load].



% Debug flag.
:- use_module(library(debug)).

:- debug(assoc_ext).
:- debug(inode).
:- debug(inodes_evaluate).
:- debug(iotw).
:- debug(oaei).

:- use_module(iotw(experiments/iotw_iimb)).
:- gtrace, iimb_experiment.

