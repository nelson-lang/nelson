%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% T.(name)(indices) = value and T.(n)(indices) = value behave like
% T.name(indices) = value: rows past the height grow the table, a name that
% is not a variable creates it
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
s = 'A';
lastwarn('');
try
  T.(s)(4) = 3;
  msg = '';
catch e
  msg = e.message;
end
assert_isequal(msg, '');
[~, id] = lastwarn();
assert_isequal(id, 'Nelson:table:rowsAddedExistingVars');
assert_isequal(T.A, [1; 2; 0; 3]);
assert_isequal(T.B, [3; 4; 0; 0]);
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
try
  T.(1)(4) = 9;
  msg = '';
catch e
  msg = e.message;
end
assert_isequal(msg, '');
assert_isequal(T.A, [1; 2; 0; 9]);
assert_isequal(T.B, [3; 4; 0; 0]);
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
s = 'Foo';
try
  T.(s)(3) = 3;
  msg = '';
catch e
  msg = e.message;
end
assert_isequal(msg, '');
assert_isequal(T.Properties.VariableNames, {'A', 'B', 'Foo'});
assert_isequal(T.Foo, [0; 0; 3]);
assert_isequal(T.A, [1; 2; 0]);
%=============================================================================
% the variable that follows the last one, a string name, braces
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
try
  T.(3)(2) = 7;
  T.("B")(1) = 5;
  T.C = {1; 2};
  T.("C"){3} = 'z';
  msg = '';
catch e
  msg = e.message;
end
assert_isequal(msg, '');
assert_isequal(T.Var3, [0; 7; 0]);
assert_isequal(T.B, [5; 4; 0]);
assert_isequal(T.C, {1; 2; 'z'});
%=============================================================================
% within the height nothing is added
T = table([1; 2; 3], 'VariableNames', {'A'});
s = 'A';
T.(s)(2) = 8;
assert_isequal(T.A, [1; 8; 3]);
T.(s)([1 3]) = [5; 6];
assert_isequal(T.A, [5; 8; 6]);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'X'});
try
  TT.(1)(3) = 5;
  s = 'Y';
  TT.(s)(2) = 8;
  msg = '';
catch e
  msg = e.message;
end
assert_isequal(msg, '');
assert_isequal(TT.X, [1; 2; 5]);
assert_isequal(TT.Y, [0; 8; 0]);
assert_istrue(isnan(seconds(TT.Properties.RowTimes(3))));
%=============================================================================
% struct fields keep their per-level assignment
st.A = [1; 2];
s = 'A';
st.(s)(4) = 3;
assert_isequal(st.A, [1; 2; 0; 3]);
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
assert_checkerror('T.(9)(1) = 1;', _('Cannot create a table variable with a discontiguous index.'), 'Nelson:table:discontiguousVars');
assert_checkerror('T.(0)(1) = 1;', _('A variable index must be a real positive integer.'), 'Nelson:table:illegalVarIndex');
assert_checkerror('T.({1})(1) = 1;', _('A table variable name must be a string scalar or a character vector.'), 'Nelson:table:illegalVarSubscript');
assert_checkerror('T.A = [1; 2; 3];', _('Value assignment must be same size as existing value.'));
%=============================================================================
