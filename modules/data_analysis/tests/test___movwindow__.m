%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sliding statistic of every column: row r covers rows max(1, r - before) .. min(n, r + after)
x = [3 1 4 1 5 9 2 6]';
asserts.isequal(__movwindow__(x, 1, 1, 1, 8, false, 'median'), [2 3 1 4 5 5 6 4]');
asserts.isequal(__movwindow__(x, 1, 1, 2, 7, false, 'median'), [3 1 4 5 5 6]');
asserts.isequal(__movwindow__(x, 2, 0, 1, 8, false, 'median'), [3 2 3 1 4 5 5 6]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 8, false, 'sum'), [4 8 6 10 15 16 17 8]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 8, false, 'mean'), [2 8 / 3 2 10 / 3 5 16 / 3 17 / 3 4]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 8, true, 'min'), [1 1 1 1 1 2 2 2]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 8, true, 'max'), [3 4 4 5 9 9 9 6]');
%=============================================================================
% a NaN in the window gives NaN, unless NaN values are omitted
x = [1 NaN 3 4]';
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, false, 'median'), [NaN NaN NaN 3.5]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, true, 'median'), [1 2 3.5 3.5]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, false, 'sum'), [NaN NaN NaN 7]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, true, 'sum'), [1 4 7 7]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, false, 'mean'), [NaN NaN NaN 3.5]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, true, 'mean'), [1 2 3.5 3.5]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, false, 'min'), [NaN NaN NaN 3]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, true, 'min'), [1 1 3 3]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 4, true, 'max'), [1 3 4 4]');
x = [NaN NaN]';
asserts.isequal(__movwindow__(x, 1, 1, 1, 2, true, 'median'), [NaN NaN]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 2, true, 'sum'), [0 0]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 2, true, 'mean'), [NaN NaN]');
asserts.isequal(__movwindow__(x, 1, 1, 1, 2, true, 'min'), [NaN NaN]');
%=============================================================================
% median absolute deviation of every window
x = [1 2 4 8 16]';
asserts.isequal(__movwindow__(x, 2, 2, 1, 5, false, 'mad'), [1 1.5 3 3 4]');
%=============================================================================
% results are double; the median of two integer values rounds half away from zero
y = __movwindow__(int8([1 2 4 7])', 0, 1, 1, 4, false, 'median');
asserts.isequal(class(y), 'double');
asserts.isequal(y, [2 3 6 7]');
asserts.isequal(__movwindow__(int8([-1 -2])', 0, 1, 1, 2, false, 'median'), [-2 -2]');
% integer sums do not saturate, as sum does
asserts.isequal(__movwindow__(int8([100 100])', 0, 1, 1, 2, false, 'sum'), [200 100]');
asserts.isequal(__movwindow__(int8([100 101])', 0, 1, 1, 1, false, 'mean'), 100.5);
% integer deviations use the saturated arithmetic of the language
x = uint8([10 200])';
asserts.isequal(__movwindow__(x, 0, 1, 1, 2, false, 'mad'), [48 0]');
asserts.isequal(__movwindow__(x, 0, 1, 1, 1, false, 'mad'), double(median(abs(x - median(x)))));
%=============================================================================
% single values: the sum accumulates in single precision, the median keeps it,
% the mean accumulates in double and is returned in single precision
x = single([1e8 1 -1e8])';
asserts.isequal(__movwindow__(x, 0, 2, 1, 1, false, 'sum'), double(sum(x)));
asserts.isequal(__movwindow__(x, 0, 2, 1, 1, false, 'sum'), 0);
asserts.isequal(__movwindow__(x, 0, 2, 1, 1, false, 'mean'), double(single(1 / 3)));
asserts.isequal(__movwindow__(single([1 2])', 0, 1, 1, 2, false, 'median'), [1.5 2]');
%=============================================================================
% every column at once, and an empty row range
asserts.isequal(__movwindow__([1 2; 3 4; 5 6], 1, 0, 1, 3, false, 'median'), [1 2; 2 3; 4 5]);
asserts.isequal(__movwindow__([1 2; 3 4; 5 6], 1, 0, 1, 3, false, 'sum'), [1 2; 4 6; 8 10]);
asserts.isequal(__movwindow__([1 2; 3 4; 5 6], 1, 0, 3, 2, false, 'median'), zeros(0, 2));
asserts.isequal(__movwindow__(zeros(3, 0), 1, 0, 1, 3, false, 'median'), zeros(3, 0));
%=============================================================================
