-module(main).
-export([main/0, process_file/1]).

process_file(FileName) ->
  {ok, IO} = file:open(FileName, [read]),
  List = process_file(io:get_line(IO, ''), IO, []),
  F = fun(Ele, Acc) ->
    {Val, _Reason}  = string:to_integer(Ele),
    if
      Val == error ->
        Acc;
      true ->
        [Val | Acc]
    end
  end,
  lists:foldl(F,  [], List).

process_file(eof, _IO, Acc) -> lists:reverse(Acc);
process_file({error, _Error}, _IO, Acc) -> lists:reverse(Acc);
process_file(Line, IO, Acc) ->
  Nums = string:tokens(string:trim(Line), ","),
  process_file(io:get_line(IO, ''), IO, Nums ++ Acc).

create_map_from_list(List) ->
  Acc = maps:new(),
  create_map_from_list(List, Acc, 0).

create_map_from_list([], Acc, _Index) -> Acc;
create_map_from_list([H | T], Acc, Index) ->
  NextAcc = maps:put(Index, H, Acc),
  NextIndex = Index + 1,
  %io:fwrite("~p~n", [NextAcc]),
  create_map_from_list(T, NextAcc, NextIndex).
%inser_into_list(List, Index, Ele) ->

% Runs a program
run_program(Program) ->
  run_program(Program, 0).

% Runs a program
run_program(Program, CurrPos) ->
  Opcode = maps:get(CurrPos, Program, 99), % 99 is exit code
  if
    Opcode == 1 ->
      NewVal = maps:get(maps:get(CurrPos + 1, Program), Program, 0) + maps:get(maps:get(CurrPos + 2, Program), Program, 0),
      ProgramNext = maps:update(maps:get(CurrPos + 3, Program), NewVal, Program),
      run_program(ProgramNext, CurrPos + 4);
    Opcode == 2 ->
      NewVal = maps:get(maps:get(CurrPos + 1, Program), Program, 0) * maps:get(maps:get(CurrPos + 2, Program), Program, 0),
      ProgramNext = maps:update(maps:get(CurrPos + 3, Program), NewVal, Program),
      run_program(ProgramNext, CurrPos + 4);
    Opcode == 99 ->
      Program
    end.

% Runs a program with a specified Noun and Verb
run_program(Program, Noun, Verb) ->
  ProgramNoun = maps:update(1, Noun, Program),
  ProgramNounVerb = maps:update(2, Verb, ProgramNoun),
  run_program(ProgramNounVerb).

% Iterate through the test cases
test_for(Program, Result) ->
  test_with_increase_indices(Program, Result, 0, 0).

% Test Programs while increasing Index1 and index2 to go through all combinations
test_with_increase_indices(_Program, _Result, Index1, _Index2) when Index1 > 99 ->
  {-1, -1};
test_with_increase_indices(Program, Result, Index1, Index2) ->
  Res = test_with_increase_index2(Program, Result, Index1, Index2),

  if
    Res == {-1, -1} ->
      NewIndex = Index1 + 1,
      test_with_increase_indices(Program, Result, NewIndex, Index2);
    true->
      Res
  end.

% Test Program with with increasing Index2 which is the Verb
test_with_increase_index2(_Program, _Result, _Index1, Index2) when Index2 > 99 ->
  {-1, -1};
test_with_increase_index2(Program, Result, Index1, Index2) ->
  Res = test_iter(Program, Result, Index1, Index2),
  NewIndex  = Index2 + 1,
  if
    Res == {-1, -1} ->
      test_with_increase_index2(Program, Result, Index1, NewIndex);
    true->
      Res
    end.



% Test specific program run gets the result we are looking for, and return true if we do
test_iter(Program, Result, Noun, Verb) ->
  ProgramEnd = run_program(Program, Noun, Verb),
  CurrRes = maps:get(0, ProgramEnd),
  if
    Result == CurrRes ->
      %io:fwrite("CurrRes[~p, ~p]: ~p~n", [Noun, Verb, CurrRes]),
      {Noun, Verb};
    true ->
      {-1, -1}
    end.



% Saves Result to a file called FileName
save_result(FileName, Result) ->
  {ok, File} = file:open(FileName, [write]),
  io:format(File, "~p~n", [Result]),
  file:close(File).

main() ->
  List = process_file("part1.in"),
  ProgramBase = create_map_from_list(List),
  ProgramEnd = run_program(ProgramBase, 12, 2),
  Result1 = maps:get(0, ProgramEnd),

  save_result("part1.out", Result1),
  io:fwrite("Part 1 result: ~p~n", [Result1]),
  % Part2
  Result2 = test_for(ProgramBase, 19690720),
  save_result("part2.out", Result2),
  io:fwrite("Part 2 result: ~p~n", [Result2]).
