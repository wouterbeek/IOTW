:- module(
  iotw_web,
  [
    load_iimb_web/2, % +Integer:integer
                     % -SVG:dom
    register_iotw/0
  ]
).

/** <module> IOTW Web

@author Wouter Beek
@version 2013/05
*/



load_iimb_web(Integer, SVG):-
  load_shared_iimb(Integer, SVG).

register_iotw:-
  register_module(iotw_web).

