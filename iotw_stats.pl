:- module(iotw_stats, []).

:- use_module(library(aggregate)).
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(prolog_pack)).
:- catch(use_module(library(real)), _, ignore(pack_install(real))).

:- initialization(iotw_stats).

iotw_stats:-
  absolute_file_name(
    '/home/wbeek/Git/PraSem/IOTW/stats_smoothing_5',
    File,
    [access(read)]
  ),
  csv_read_file(File, Rows, [arity(6)]),
  
  findall(
    I-Triples,
    (
      % Argument
      between(1, 5, I),
      findall(
        J-Values-Avg,
        (
          % Precision
          between(0, 9, J),
          findall(
            Value,
            (
              nth0(K, Rows, Row),
              Row =.. [row|Args],
              J =:= (K mod 10),
              nth0(I, Args, Value)
            ),
            Values
          ),
          avgerage(Values, Avg)
        ),
        Triples
      )
    ),
    Results
  ),
  forall(
    member(I-Triples, Results),
    (
      argument(I, ArgLabel),
      write('Argument:'), write(I), write(' ('), write(ArgLabel), write(')'), nl,
      forall(
        member(J-_-Avg, Triples),
        (write('Stage:'), write(J), write('::'), write(Avg), nl)
      )
    )
  ),
  halt.

argument(0, 'Percentage').
argument(1, 'Lower recall').
argument(2, 'Higher recall').
argument(3, 'Quality').
argument(4, 'Higher cover').
argument(5, 'Removed identity pairs in higher').

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

avgerage(Numbers, Average):-
  sum_list(Numbers, Sum),
  length(Numbers, NumberOfNumbers),
  Average is Sum / NumberOfNumbers.
