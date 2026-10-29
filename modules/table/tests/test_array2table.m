%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 4 7; 2 5 8; 3 6 9];
T = array2table(A);
asserts.istrue(istable(T));
asserts.isequal(size(T), [3, 3]);
asserts.isequal(T.A1, [1; 2; 3]);
asserts.isequal(T.A2, [4; 5; 6]);
asserts.isequal(T.A3, [7; 8; 9]);
%=============================================================================
T = array2table([1 4 7; 2 5 8; 3 6 9]);
asserts.istrue(istable(T));
asserts.isequal(size(T), [3, 3]);
asserts.isequal(T.Var1, [1; 2; 3]);
asserts.isequal(T.Var2, [4; 5; 6]);
asserts.isequal(T.Var3, [7; 8; 9]);
%=============================================================================
% VariableNames and RowNames name-value options
T = array2table([1 2; 3 4], 'VariableNames', {'a', 'b'});
assert_isequal(T.a, [1; 3]);
assert_isequal(T.b, [2; 4]);
assert_isequal(T.Properties.VariableNames, {'a', 'b'});
%=============================================================================
T = array2table([1 2; 3 4], 'VariableNames', {'x', 'y'}, 'RowNames', {'r1', 'r2'});
assert_istrue(all(ismember({'r1', 'r2'}, T.Properties.RowNames)));
assert_isequal(T{'r2', 'x'}, 3);
%=============================================================================
% RowNames is stored as a column (N x 1), including from a column input.
T = array2table([1 2; 3 4], 'RowNames', {'x'; 'y'});
assert_isequal(size(T.Properties.RowNames), [2 1]);
assert_isequal(T.Properties.RowNames, {'x'; 'y'});
T = array2table([1 2; 3 4], 'RowNames', {'x', 'y'});
assert_isequal(size(T.Properties.RowNames), [2 1]);
%=============================================================================
% a string array is accepted for VariableNames
T = array2table([5 6], 'VariableNames', ["p" "q"]);
assert_isequal(T.Properties.VariableNames, {'p', 'q'});
%=============================================================================
assert_checkerror('array2table([1 2], ''BadName'', 1)', _('Unknown parameter name.'));
%=============================================================================
