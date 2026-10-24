%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_table_disp_cell_string_padding()
  % <missing> string element: no quotes, no padding
  T = table(["x"; string(missing)]);
  assert_lines(T, ["""x"""; "<missing>"]);
  %=============================================================================
  % rows added by growth hold <missing>
  T = table([1; 2], ["a"; "bb"], 'VariableNames', {'n', 's'});
  T{4, 1} = 9;
  L = display_lines(T);
  assert_isequal(L(end - 1:end), ["0    <missing>"; "9    <missing>"]);
  %=============================================================================
  % cell contents padded to a common width, numbers right-aligned
  T = table({'a'; zeros(0, 0); 1; 'hello'; "s"; -1; 3.25});
  sz = ['0', char(215), '0'];
  assert_lines(T, ["{'a'       }"; "{" + sz + " double}"; "{[       1]}"; ...
    "{'hello'   }"; "{[""s""     ]}"; "{[      -1]}"; "{[  3.2500]}"]);
  %=============================================================================
  T = table({'a'; 'bb'}, 'RowNames', {'r1'; 'r2'});
  assert_lines(T, ["r1    {'a' }"; "r2    {'bb'}"]);
  %=============================================================================
  % each column of a multi-column variable has its own width
  T = table({'a', 1; 'bb', 22});
  assert_lines(T, ["{'a' }    {[ 1]}"; "{'bb'}    {[22]}"]);
  T = table(["a", "b"; string(missing), "cc"]);
  assert_lines(T, ["""a""          ""b"""; "<missing>    ""cc"""]);
  %=============================================================================
  TT = timetable(seconds([1; 2]), [string(missing); "x"], {'a'; 22});
  assert_lines(TT, ["1 sec    <missing>    {'a' }"; "2 sec    ""x""          {[22]}"]);
  %=============================================================================
  % compact display (more than 20 rows)
  T = table((1:22)', [repmat("s", 21, 1); string(missing)], [{'a'}; repmat({1}, 21, 1)]);
  L = display_lines(T);
  assert_isequal(L(3), "1     ""s""          {'a'}");
  assert_isequal(L(4), "2     ""s""          {[1]}");
  assert_isequal(L(end), "22     <missing>    {[1]}");
end
%=============================================================================
function assert_lines(T, expected)
  L = display_lines(T);
  assert_isequal(L(end - numel(expected) + 1:end), expected(:));
end
%=============================================================================
function L = display_lines(T)
  L = strtrim(splitlines(string(evalc('disp(T)'))));
  L(L == "") = [];
end
%=============================================================================
