%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% duration array text (string/char/cellstr/display) is rendered in one
% vectorized pass; it must match the element-by-element rendering for every
% format, including the five-significant-digit number text and NaN/Inf.
%=============================================================================
% Unit format 's': singular/plural is 'sec'; five significant digits with the
% notation chosen from the magnitude.
s = string(duration.fromSeconds([1 22 333 1.5 123456 0.00001 99999.5], 's'));
assert_isequal(s, ["1 sec", "22 sec", "333 sec", "1.5 sec", ...
  "1.2346e+05 sec", "1e-05 sec", "100000 sec"]);
%=============================================================================
% Unit format 'd'/'h': singular only when the scaled magnitude is exactly 1.
assert_isequal(string(duration.fromSeconds([86400 129600], 'd')), ["1 day", "1.5 days"]);
assert_isequal(string(duration.fromSeconds([3600 5400 -3600], 'h')), ["1 hr", "1.5 hr", "-1 hr"]);
%=============================================================================
% Clock formats.
assert_isequal(string(duration.fromSeconds([3661 90061 -3661], 'hh:mm:ss')), ...
  ["01:01:01", "25:01:01", "-01:01:01"]);
assert_isequal(string(duration.fromSeconds([90061 -90061], 'dd:hh:mm:ss')), ...
  ["01:01:01:01", "-01:01:01:01"]);
assert_isequal(string(duration.fromSeconds(3661.5, 'hh:mm:ss.SSS')), "01:01:01.500");
assert_isequal(string(duration.fromSeconds(3661, 'hh:mm')), "01:01");
assert_isequal(string(duration.fromSeconds(3661, 'mm:ss')), "61:01");
%=============================================================================
% NaN -> <missing> in string(), the text 'NaN'/'Inf'/'-Inf' in cellstr.
d = duration.fromSeconds([NaN Inf -Inf 3661], 'hh:mm:ss');
s = string(d);
assert_isequal(ismissing(s), [true false false false]);
assert_isequal(s(2), "Inf");
assert_isequal(s(3), "-Inf");
c = cellstr(d);
assert_isequal(c{1}, 'NaN');
assert_isequal(c{4}, '01:01:01');
% NaN in a unit format keeps the unit suffix.
assert_isequal(cellstr(duration.fromSeconds(NaN, 's')), {'NaN sec'});
%=============================================================================
% Shapes preserved.
d = duration.fromSeconds(reshape((1:24) * 3600, 2, 3, 4), 'hh:mm:ss');
assert_isequal(size(string(d)), [2 3 4]);
assert_isequal(size(string(duration.fromSeconds(zeros(0, 2), 's'))), [0 2]);
%=============================================================================
