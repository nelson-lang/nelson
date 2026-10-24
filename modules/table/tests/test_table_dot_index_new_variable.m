%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% T.name(indices) = value on a name that is not a variable creates a column
% of the default value of the class of value; rows past the height grow the
% table
%=============================================================================
T = table([1; 2; 3]);
lastwarn('');
T.New(2) = "x";
[msg, id] = lastwarn();
assert_isequal(id, '');
assert_isequal(T.New, [string(missing); "x"; string(missing)]);
assert_isequal(T.Properties.VariableNames, {'Var1', 'New'});
assert_isequal(T.Var1, [1; 2; 3]);
%=============================================================================
T = table([1; 2; 3]);
T.D(2) = 5;
assert_isequal(T.D, [0; 5; 0]);
T.L(3) = true;
assert_isequal(T.L, [false; false; true]);
T.I(2) = int8(4);
assert_isequal(T.I, int8([0; 4; 0]));
T.W(2, 2) = 5;
assert_isequal(T.W, [0 0; 0 5; 0 0]);
T.R(2:3) = [7 8];
assert_isequal(T.R, [0; 7; 8]);
T.A(:) = 4;
assert_isequal(T.A, [4; 4; 4]);
T.B([1 3]) = [7; 8];
assert_isequal(T.B, [7; 0; 8]);
%=============================================================================
T = table([1; 2; 3]);
T.C{2} = 'a';
assert_isequal(T.C, {[]; 'a'; []});
T.K(2) = {'b'};
assert_isequal(T.K, {[]; 'b'; []});
%=============================================================================
T = table([1; 2; 3]);
T.T(2) = datetime(2020, 1, 1);
assert_istrue(isnat(T.T(1)));
assert_isequal(T.T(2), datetime(2020, 1, 1));
T.G(2) = categorical("u");
assert_isequal(cellstr(T.G(2)), {'u'});
assert_istrue(all(isundefined(T.G([1 3]))));
%=============================================================================
T = table([1; 2; 3], 'RowNames', {'a'; 'b'; 'c'});
T.D(2) = 4;
assert_isequal(T{'b', 'D'}, 4);
assert_isequal(T.D, [0; 4; 0]);
%=============================================================================
% rows past the height grow the table
T = table([1; 2; 3]);
lastwarn('');
T.D(5) = 5;
[msg, id] = lastwarn();
assert_isequal(id, 'Nelson:table:rowsAddedExistingVars');
assert_isequal(height(T), 5);
assert_isequal(T.D, [0; 0; 0; 0; 5]);
assert_isequal(T.Var1, [1; 2; 3; 0; 0]);
%=============================================================================
T = table([1; 2; 3]);
lastwarn('');
T.Var1(4) = 9;
[msg, id] = lastwarn();
assert_isequal(id, '');
assert_isequal(T.Var1, [1; 2; 3; 9]);
assert_isequal(height(T), 4);
%=============================================================================
T = table([1; 2; 3], 'RowNames', {'a'; 'b'; 'c'});
T.D(4) = 1;
assert_isequal(T.Properties.RowNames, {'a'; 'b'; 'c'; 'Row4'});
%=============================================================================
T = table();
T.D(2) = 9;
assert_isequal(T.D, [0; 9]);
assert_isequal(height(T), 2);
%=============================================================================
% an existing variable is still indexed as before
T = table([1; 2; 3]);
T.Var1(2) = 9;
assert_isequal(T.Var1, [1; 9; 3]);
T.Var1(end) = 7;
assert_isequal(T.Var1, [1; 9; 7]);
T.Var1(end + 1) = 6;
assert_isequal(T.Var1, [1; 9; 7; 6]);
%=============================================================================
% timetable
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
TT.New(2) = "x";
assert_isequal(TT.New, [string(missing); "x"; string(missing)]);
TT.D(3) = 4;
assert_isequal(TT.D, [0; 0; 4]);
TT.Time(2) = seconds(5);
assert_isequal(TT.Time, seconds([1; 5; 3]));
%=============================================================================
% inside a function (local variable)
R = localNewVariable();
assert_isequal(R.New, [3; 0]);
%=============================================================================
assert_checkerror('T = table([1; 2; 3]); T.D(end) = 9;', _('Unrecognized table variable name ''D''.'), 'Nelson:table:unrecognizedVariable');
try
  T = table([1; 2; 3]);
  T.D(2) = [1 2];
  id = '';
catch e
  id = e.identifier;
end
assert_isequal(id, 'Nelson:dimagree:sizeMismatchAssignmentAI');
%=============================================================================
% other objects and structs keep the per-level assignment
s.a = [1 2 3];
s.a(2) = 5;
assert_isequal(s.a, [1 5 3]);
s.b(3) = 1;
assert_isequal(s.b, [0 0 1]);
s.c{2} = 'z';
assert_isequal(s.c, {[], 'z'});
%=============================================================================
function R = localNewVariable()
  R = table([1; 2]);
  R.New(1) = 3;
end
%=============================================================================
