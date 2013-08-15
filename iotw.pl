:- module(
  iotw,
  [
  ]
).

/** <module> IOTW

Coordinates the Itentity on the Web experiment.

@author Wouter Beek
@version 2013/08
*/

:- use_module(iotw(iimb)).

%:- initialization(run_experiment).



run_experiment:-
  gtrace,
  load_alignment_iimb(1, SVG),
  write(SVG).

