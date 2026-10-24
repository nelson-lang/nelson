%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression coverage for the vectorized retime/synchronize implementation:
% positional previous/next on unsorted times, duplicate times, aggregation
% order, EndValues and multi-column variables.
%=============================================================================
% previous/next use the last/first old row by original position, not temporal
% order, when the row times are unsorted.
TTu = timetable(seconds([3; 1; 2; 5]), [30; 10; 20; 50], 'VariableNames', {'A'});
assert_isequal(retime(TTu, seconds([1.5; 4]), 'previous').A, [10; 20]);
assert_isequal(retime(TTu, seconds([1.5; 4]), 'next').A, [30; 50]);
%=============================================================================
% nearest breaks distance ties by lowest original row index.
assert_isequal(retime(TTu, seconds([1.4; 3.6]), 'nearest').A, [10; 30]);
%=============================================================================
% Duplicate old times: aggregation keeps original row order for first/last.
TTd = timetable(seconds([1; 2; 2; 3]), [10; 20; 25; 30], 'VariableNames', {'A'});
assert_isequal(retime(TTd, seconds([1; 2.5]), 'firstvalue').A, [10; 30]);
assert_isequal(retime(TTd, seconds([1; 2.5]), 'lastvalue').A, [25; 30]);
assert_isequal(retime(TTd, seconds([1; 2.5]), 'sum').A, [55; 30]);
assert_isequal(retime(TTd, seconds([1; 2.5]), 'mean').A, [55 / 3; 30]);
assert_isequal(retime(TTd, seconds([1; 2.5]), 'count').A, [3; 1]);
%=============================================================================
% Empty bins stay missing (except count -> 0).
R = retime(TTd, seconds([1; 2.5; 10]), 'sum');
assert_istrue(isnan(R.A(3)));
R = retime(TTd, seconds([1; 2.5; 10]), 'count');
assert_isequal(R.A(3), 0);
%=============================================================================
% Multi-column linear interpolation with a constant EndValues outside the range.
TTm = timetable(seconds([1; 2; 4]), [10 100; 20 200; 40 400], 'VariableNames', {'A'});
R = retime(TTm, seconds([0; 3; 5]), 'linear', 'EndValues', -1);
assert_isequal(R.A, [-1 -1; 30 300; -1 -1]);
R = retime(TTm, seconds([0; 3; 5]), 'linear');
assert_isequal(R.A, [0 0; 30 300; 50 500]);
%=============================================================================
% Integer-class interpolation preserves the integer class (per-point path).
TTi = timetable(seconds([1; 2; 3]), int16([10; 20; 30]), 'VariableNames', {'A'});
R = retime(TTi, seconds([1.5; 2.5]), 'linear');
assert_isequal(class(R.A), 'int16');
assert_isequal(R.A, int16([15; 25]));
%=============================================================================
% Exact matches win over the method for every method and take the first match.
TT = timetable(seconds([1; 2; 3]), [10; 20; 30], 'VariableNames', {'A'});
assert_isequal(retime(TT, seconds([1; 1.5; 2]), 'linear').A, [10; 15; 20]);
assert_isequal(retime(TT, seconds([1; 1.5; 4]), 'previous').A, [10; 10; 30]);
%=============================================================================
% synchronize delegates to retime and matches the linear union result.
Left = timetable(seconds([1; 3]), [10; 30], 'VariableNames', {'A'});
Right = timetable(seconds([2; 3]), [200; 300], 'VariableNames', {'B'});
U = synchronize(Left, Right, 'union', 'linear');
assert_isequal(U.A, [10; 20; 30]);
assert_isequal(U.B, [100; 200; 300]);
%=============================================================================
