:- module(iotw_stats, []).

:- use_module(library(aggregate)).
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(prolog_pack)).
:- catch(use_module(library(real)), _, ignore(pack_install(real))).
:- use_module(library(sgml)).

:- initialization(iotw_stats).

iotw_stats:-
  absolute_file_name(
    '/home/wbeek/Git/PraSem/IOTW/stats_test',
    File,
    [access(read)]
  ),
  csv_read_file(File, Rows, [arity(6)]),
  
  NumberOfSteps = 10,
  length(Rows, NumberOfRows),
  NumberOfExperiments is NumberOfRows / NumberOfSteps,
  
  findall(
    Pairs,
    (
      between(1, 5, I),
      findall(
        Avg,
        (
          member(Perc, [1.0,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1]),
          aggregate_all(
            sum(Value),
            (
              member(Row, Rows),
              Row =.. [row,Perc|Args],
              nth1(I, Args, Value)
            ),
            Sum
          ),
          Avg is Sum / NumberOfExperiments
        ),
        Pairs
      )
    ),
    [LRecalls,HRecalls,Q2s,HCovers,H_IPairs3_Percs]
  ),
  
  writeln(LRecalls),
  writeln(HRecalls),
  writeln(Q2s),
  writeln(HCovers),
  writeln(H_IPairs3_Percs).

plot(Coords):-
  absolute_file_name(
    '/home/wbeek/Git/PraSem/IOTW/plot.svg',
    File,
    [access(write)]
  ),
  <- svg(+File),
  coords <- Coords,
  <- plot(coords),
  <- dev..off(.).
