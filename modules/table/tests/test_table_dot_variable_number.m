%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% T.(n): a number as dynamic field name is the variable number n. The
% errors are checked with try/catch: assert_checkerror does not evaluate
% through the same indexing paths as a statement.
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
assert_isequal(T.(1), [1; 2]);
assert_isequal(T.(int8(2)), [3; 4]);
n = 2;
assert_isequal(T.(n), [3; 4]);
f = @(t) t.(2);
assert_isequal(f(T), [3; 4]);
%=============================================================================
T2 = T;
T2.(2) = [7; 8];
assert_isequal(T2.B, [7; 8]);
T2.(2) = {5; 6};
assert_isequal(T2.B, {5; 6});
T2 = T;
T2.(3) = [5; 6];
assert_isequal(T2.Properties.VariableNames, {'A', 'B', 'Var3'});
assert_isequal(T2.Var3, [5; 6]);
T2 = T;
T2.(1) = [];
assert_isequal(T2.Properties.VariableNames, {'B'});
T2 = table([1; 2], [3; 4], 'VariableNames', {'Var3', 'B'});
T2.(3) = [5; 6];
assert_isequal(T2.Properties.VariableNames, {'Var3', 'B', 'Var3_1'});
%=============================================================================
msg = '';
try
  x = T.(3);
catch e
  msg = e.message;
end
assert_isequal(msg, _('Variable index exceeds table dimensions.'));
msg = '';
try
  T2 = T;
  T2.(4) = [5; 6];
catch e
  msg = e.message;
end
assert_isequal(msg, _('Cannot create a table variable with a discontiguous index.'));
msg = '';
try
  x = T.(1.5);
catch e
  msg = e.message;
end
assert_isequal(msg, _('A variable index must be a real positive integer.'));
msg = '';
try
  T2 = T;
  T2.(0) = [5; 6];
catch e
  msg = e.message;
end
assert_isequal(msg, _('A variable index must be a real positive integer.'));
msg = '';
try
  T2 = T;
  T2.(true) = [5; 6];
catch e
  msg = e.message;
end
assert_isequal(msg, _('A table variable name must be a string scalar or a character vector.'));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'A'});
assert_isequal(TT.(1), [1; 2]);
TT2 = TT;
TT2.(1) = [8; 9];
assert_isequal(TT2.A, [8; 9]);
TT2.(2) = [5; 6];
assert_isequal(TT2.Properties.VariableNames, {'A', 'Var2'});
assert_isequal(TT2.Time, seconds([1; 2]));
msg = '';
try
  x = TT.(2);
catch e
  msg = e.message;
end
assert_isequal(msg, _('Variable index exceeds table dimensions.'));
msg = '';
try
  TT2 = TT;
  TT2.(3) = [8; 9];
catch e
  msg = e.message;
end
assert_isequal(msg, _('Cannot create a table variable with a discontiguous index.'));
%=============================================================================
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
assert_isequal(ET.(1), ["a"; "b"]);
ET.(2) = [4; 5];
assert_isequal(ET.Var2, [4; 5]);
assert_isequal(class(ET), 'eventtable');
%=============================================================================
s.a = 1;
msg = '';
try
  x = s.(1);
catch e
  msg = e.message;
end
assert_isequal(msg, _('Dynamic field name must be a string.'));
%=============================================================================
