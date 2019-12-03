-module(main).
-export([main/0, process_file/1]).

process_file(FileName) ->
  {ok, IO} = file:open(FileName, [read]),
  process_file(io:get_line(IO, ''), IO, []).

process_file(eof, _IO, Acc) -> lists:reverse(Acc);
process_file({error, _Error}, _IO, Acc) -> lists:reverse(Acc);
process_file(Line, IO, Acc) ->
  {Num, Rest} = string:to_integer(string:trim(Line)),
  if
    Num == error ->
      process_file(io:get_line(IO, ''), IO, Acc);
    true ->
      process_file(io:get_line(IO, ''), IO, [Num] ++ Acc)
  end.

process_ele(Ele) ->
  (Ele div 3) - 2.

calc_total_mass(Mass) ->
  FuelMass = process_ele(Mass),
  if
    FuelMass > 0 ->
      FuelMass + calc_total_mass(FuelMass);
    true ->
      0
    end.



main() ->
  List = process_file("input.in"),

  F = fun(Ele, Acc) ->
    Val = calc_total_mass(Ele),
    %io:fwrite("~w\n", [Val]),
    [Val | Acc] end,

  FuelVals = lists:foldr(F, [], List),

  FuelSum = lists:sum(FuelVals),

  {ok, File} = file:open("part2.out", [write]),
  % file:write_file("output.out", [string:to_string(FuelSum) ++ "\n"]).
  io:format(File, "~w", [FuelSum]),
  file:close(File).
