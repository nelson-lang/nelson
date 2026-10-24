%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique on tables: 'sorted' (default) / 'stable' order, 'first' / 'last'
%=============================================================================
T = table([3; 1; 3; 2; 1], {'b'; 'a'; 'b'; 'c'; 'a'}, 'VariableNames', {'X', 'S'});
[C, ia, ic] = unique(T);
assert_isequal(C.X, [1; 2; 3]);
assert_isequal(C.S, {'a'; 'c'; 'b'});
assert_isequal(ia, [2; 4; 1]);
assert_isequal(ic, [3; 1; 3; 2; 1]);
assert_isequal(C, T(ia, :));
assert_isequal(T, C(ic, :));
%=============================================================================
[C2, ia2, ic2] = unique(T, 'sorted');
assert_isequal(C2, C);
assert_isequal(ia2, ia);
assert_isequal(ic2, ic);
[C2, ia2, ic2] = unique(T, 'first');
assert_isequal(C2, C);
assert_isequal(ia2, ia);
assert_isequal(ic2, ic);
C2 = unique(T, 'rows');
assert_isequal(C2, C);
%=============================================================================
[C, ia, ic] = unique(T, 'stable');
assert_isequal(C.X, [3; 1; 2]);
assert_isequal(C.S, {'b'; 'a'; 'c'});
assert_isequal(ia, [1; 2; 4]);
assert_isequal(ic, [1; 2; 1; 3; 2]);
%=============================================================================
[C, ia, ic] = unique(T, 'last');
assert_isequal(C.X, [1; 2; 3]);
assert_isequal(ia, [5; 4; 3]);
assert_isequal(ic, [3; 1; 3; 2; 1]);
%=============================================================================
[C, ia, ic] = unique(T, 'stable', 'last');
assert_isequal(C.X, [3; 2; 1]);
assert_isequal(ia, [3; 4; 5]);
assert_isequal(ic, [1; 3; 1; 2; 3]);
%=============================================================================
% ties on the first variable are broken by the next ones
T = table([2; 1; 2; 1], [9; 8; 7; 8]);
[C, ia, ic] = unique(T);
assert_isequal(C.Var1, [1; 2; 2]);
assert_isequal(C.Var2, [8; 7; 9]);
assert_isequal(ia, [2; 3; 1]);
assert_isequal(ic, [3; 1; 2; 1]);
%=============================================================================
% mixed variable types
T = table(["z"; "a"; "z"; "m"], [true; false; true; false], categorical({'lo'; 'hi'; 'lo'; 'hi'}), datetime(2020, 1, [3; 1; 3; 2]));
[C, ia, ic] = unique(T);
assert_isequal(C.Var1, ["a"; "m"; "z"]);
assert_isequal(C.Var2, [false; false; true]);
assert_isequal(ia, [2; 4; 1]);
assert_isequal(ic, [3; 1; 3; 2]);
%=============================================================================
% multicolumn variable: columns compared left to right
T = table([1 2; 0 5; 1 2; 1 1], [-1; 3; -1; 0]);
[C, ia, ic] = unique(T);
assert_isequal(C.Var1, [0 5; 1 1; 1 2]);
assert_isequal(C.Var2, [3; 0; -1]);
assert_isequal(ia, [2; 4; 1]);
assert_isequal(ic, [3; 1; 3; 2]);
%=============================================================================
% empty char is a plain value in a cellstr variable
T = table({'b'; ''; 'a'; ''}, [1; 1; 1; 1]);
[C, ia, ic] = unique(T);
assert_isequal(C.Var1, {''; 'a'; 'b'});
assert_isequal(ia, [2; 3; 1]);
assert_isequal(ic, [3; 1; 2; 1]);
%=============================================================================
% row names follow the selected rows
T = table([2; 1; 2], 'RowNames', {'r1'; 'r2'; 'r3'});
C = unique(T);
assert_isequal(C.Properties.RowNames, {'r2'; 'r1'});
%=============================================================================
% missing values sort last and stay distinct by default
T = table(categorical({'b'; ''; 'a'; ''}), datetime(2020, 1, [3; 4; 1; 4]));
[C, ia, ic] = unique(T);
assert_isequal(ia, [3; 1; 2; 4]);
assert_isequal(ic, [2; 3; 1; 4]);
T = table(datetime(2020, 1, [3; NaN; 1; NaN]));
[C, ia, ic] = unique(T);
assert_isequal(ia, [3; 1; 2; 4]);
assert_isequal(ic, [2; 3; 1; 4]);
[C, ia, ic] = unique(T, 'TreatMissingAsDistinct', false);
assert_isequal(ia, [3; 1; 2]);
assert_isequal(ic, [2; 3; 1; 3]);
%=============================================================================
T = table(zeros(0, 1), zeros(0, 1));
[C, ia, ic] = unique(T);
assert_isequal(size(C), [0 2]);
assert_isequal(size(ia), [0 1]);
assert_isequal(size(ic), [0 1]);
%=============================================================================
T = table([1; 2; 1]);
assert_checkerror('unique(T, ''legacy'')', 'The ''R2012a'' and ''legacy'' options are not supported.');
%=============================================================================
