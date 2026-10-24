%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2; 4], [10; 20; 30], 'VariableNames', {'X', 'Y'});
R = varfun(@mean, T);
asserts.isequal(R.mean_X, 7 / 3);
asserts.isequal(R.mean_Y, 20);
%=============================================================================
% varfun on a timetable returns a timetable
t = datetime(2023, 1, 1) + days(0:4)';
TT = timetable(t, [1; 2; 3; 4; 5], [10; 20; 30; 40; 50], 'VariableNames', {'a', 'b'});
s = varfun(@sum, TT);
assert_isequal(class(s), 'timetable');
assert_isequal(s{1, :}, [15 150]);
assert_isequal(s.Properties.RowTimes(1), datetime(2023, 1, 1));
%=============================================================================
% varfun with GroupingVariables
T = table([1; 2; 1; 2], [10; 20; 30; 40], [1; 1; 1; 1], 'VariableNames', {'g', 'v', 'w'});
r = varfun(@sum, T, 'GroupingVariables', 'g');
assert_isequal(r.Properties.VariableNames, {'g', 'GroupCount', 'sum_v', 'sum_w'});
assert_isequal(r.g, [1; 2]);
assert_isequal(r.GroupCount, [2; 2]);
assert_isequal(r.sum_v, [40; 60]);
%=============================================================================
