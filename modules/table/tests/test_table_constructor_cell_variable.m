%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% table(C): a cell array argument is one variable, stored as given.
%=============================================================================
cases = {{[1 2]; [3 4]}, {'a'; 'bcd'}, {[1 2]; 3}, {1; 2}, {1 2; 3 4}, ...
  {{1}; {2}}, {"a"; "b"}, {true; false}, {1; 'a'}, cell(2, 1), cell(0, 1), ...
  {}, {[1 2]}, {1}, {1, 2}};
for k = 1:numel(cases)
  C = cases{k};
  T = table(C);
  assert_isequal(width(T), 1);
  assert_isequal(height(T), size(C, 1));
  assert_isequal(T.Properties.VariableNames, {'C'});
  assert_isequal(class(T.C), 'cell');
  assert_isequal(T.C, C);
end
%=============================================================================
T = table({[1 2]; [3 4]}, 'VariableNames', {'c'});
assert_isequal(T.Properties.VariableNames, {'c'});
assert_isequal(T.c, {[1 2]; [3 4]});
%=============================================================================
T = table({1 2; 3 4}, 'VariableNames', {'a'});
assert_isequal(size(T), [2 1]);
assert_isequal(T.a, {1 2; 3 4});
%=============================================================================
assert_checkerror('table({1 2; 3 4}, ''VariableNames'', {''a'', ''b''})', _('Mismatched variable names.'), 'Nelson:table:variableNamesMismatch');
%=============================================================================
T = table({1; 2}, [3; 4]);
assert_isequal(T.Var1, {1; 2});
assert_isequal(T.Var2, [3; 4]);
%=============================================================================
% cell2table still concatenates the elements of each column.
T = cell2table({[1 2]; [3 4]});
assert_isequal(T.Var1, [1 2; 3 4]);
T = cell2table({1 "a"; 2 "b"});
assert_isequal(T.Var1, [1; 2]);
assert_isequal(T.Var2, ["a"; "b"]);
T = cell2table({true; false});
assert_isequal(T.Var1, [true; false]);
T = cell2table({'a'; 'bcd'});
assert_isequal(T.Var1, {'a'; 'bcd'});
%=============================================================================
