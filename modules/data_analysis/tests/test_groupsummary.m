%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table({'a'; 'a'; 'b'}, [1; 2; 4], [10; 20; 30], 'VariableNames', {'G', 'X', 'Y'});
R = groupsummary(T, 'G', 'sum', {'X', 'Y'});
assert_isequal(R.sum_X, [3; 4]);
assert_isequal(R.sum_Y, [30; 30]);
%=============================================================================
% Default method is 'count'.
Tc = table({'a'; 'a'; 'b'; 'b'; 'b'}, (1:5)', 'VariableNames', {'g', 'v'});
Rc = groupsummary(Tc, 'g');
assert_isequal(Rc.Properties.VariableNames, {'g', 'GroupCount', 'count_v'});
assert_isequal(Rc.GroupCount, [2; 3]);
assert_isequal(Rc.count_v, [2; 3]);
%=============================================================================
% Each built-in method on a single data variable.
Tm = table({'a'; 'a'; 'b'; 'b'; 'b'}, [1; 3; 2; 8; 5], 'VariableNames', {'g', 'v'});
assert_isequal(groupsummary(Tm, 'g', 'mean', 'v').mean_v, [2; 5]);
assert_isequal(groupsummary(Tm, 'g', 'min', 'v').min_v, [1; 2]);
assert_isequal(groupsummary(Tm, 'g', 'max', 'v').max_v, [3; 8]);
assert_isequal(groupsummary(Tm, 'g', 'sum', 'v').sum_v, [4; 15]);
assert_isequal(groupsummary(Tm, 'g', 'count', 'v').count_v, [2; 3]);
%=============================================================================
% Multiple grouping variables keep stable (first-appearance) order.
T2 = table({'a'; 'b'; 'a'; 'b'; 'a'}, [1; 1; 2; 2; 1], (1:5)', 'VariableNames', {'g1', 'g2', 'v'});
R2 = groupsummary(T2, {'g1', 'g2'});
assert_isequal(R2.g1, {'a'; 'b'; 'a'; 'b'});
assert_isequal(R2.g2, [1; 1; 2; 2]);
assert_isequal(R2.GroupCount, [2; 1; 1; 1]);
%=============================================================================
% Function handle method over several data variables.
Tf = table({'a'; 'a'; 'b'}, [1; 3; 2], [10; 40; 20], 'VariableNames', {'g', 'x', 'y'});
Rf = groupsummary(Tf, 'g', @(z) sum(z), {'x', 'y'});
assert_isequal(Rf.Properties.VariableNames, {'g', 'GroupCount', 'fun1_x', 'fun1_y'});
assert_isequal(Rf.fun1_x, [4; 2]);
assert_isequal(Rf.fun1_y, [50; 20]);
%=============================================================================
% Function handle returning a derived scalar per group.
Rr = groupsummary(Tm, 'g', @(x) max(x) - min(x), 'v');
assert_isequal(Rr.Properties.VariableNames, {'g', 'GroupCount', 'fun1_v'});
assert_isequal(Rr.fun1_v, [2; 6]);
%=============================================================================
% timetable input: grouping drops the row times and summarizes as a table.
tt = timetable(seconds((1:5)'), {'a'; 'b'; 'a'; 'b'; 'a'}, (10:10:50)', 'VariableNames', {'g', 'v'});
Rtt = groupsummary(tt, 'g', 'sum', 'v');
assert_isequal(Rtt.sum_v, [90; 60]);
%=============================================================================
% Large-N sanity: order-independent invariants on the fast grouping path.
% (A quadratic implementation would blow the test-suite timeout here; the
% timing numbers themselves are reported by the standalone micro-benchmark.)
N = 20000;
rng(7);
g = randi(40, N, 1);
v = rand(N, 1);
TL = table(g, v, 'VariableNames', {'g', 'v'});
GL = groupsummary(TL, 'g', 'sum', 'v');
assert_isequal(sum(GL.GroupCount), N);
assert_isequal(height(GL), numel(unique(g)));
assert_istrue(abs(sum(GL.sum_v) - sum(v)) < 1e-6);
%=============================================================================
% categorical grouping variable.
Tcat = table(categorical({'x'; 'y'; 'x'}), [1; 2; 3], 'VariableNames', {'g', 'v'});
Rcat = groupsummary(Tcat, 'g', 'sum', 'v');
assert_isequal(cellstr(Rcat.g), {'x'; 'y'});
assert_isequal(Rcat.GroupCount, [2; 1]);
assert_isequal(Rcat.sum_v, [4; 2]);
%=============================================================================
% categorical + datetime grouping variables (stable first-appearance order).
Cg = categorical({'b'; 'a'; 'b'; 'a'; 'a'});
Dg = datetime(2022, 1, [2; 1; 2; 1; 3]);
Tcd = table(Cg, Dg, [10; 20; 30; 40; 50], 'VariableNames', {'c', 'd', 'v'});
Rcd = groupsummary(Tcd, {'c', 'd'}, 'sum', 'v');
assert_isequal(cellstr(Rcd.c), {'b'; 'a'; 'a'});
assert_isequal(Rcd.d, datetime(2022, 1, [2; 1; 3]));
assert_isequal(Rcd.GroupCount, [2; 2; 1]);
assert_isequal(Rcd.sum_v, [40; 60; 50]);
%=============================================================================
