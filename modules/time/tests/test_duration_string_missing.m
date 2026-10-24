%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% string() of a duration returns <missing> for NaN elements
s = string(seconds([1 NaN]));
assert_isequal(class(s), 'string');
assert_isequal(size(s), [1 2]);
assert_isequal(s(1), "1 sec");
assert_isequal(ismissing(s), [false true]);
%=============================================================================
assert_isequal(ismissing(string(minutes([1 NaN]))), [false true]);
assert_isequal(string(minutes(1)), "1 min");
assert_istrue(ismissing(string(hours(NaN))));
assert_isequal(ismissing(string(days([1 NaN]))), [false true]);
assert_isequal(ismissing(string(years([1 NaN]))), [false true]);
assert_istrue(ismissing(string(duration(NaN, 0, 0))));
s = string(seconds(NaN(2, 2)));
assert_isequal(size(s), [2 2]);
assert_istrue(all(ismissing(s(:))));
%=============================================================================
% clock display formats
d = seconds([1 NaN 3600]);
d.Format = 'hh:mm:ss';
s = string(d);
assert_isequal(s([1 3]), ["00:00:01", "01:00:00"]);
assert_isequal(ismissing(s), [false true false]);
d = seconds([1 NaN]);
d.Format = 'mm:ss';
assert_isequal(ismissing(string(d)), [false true]);
d.Format = 'hh:mm:ss.SSS';
s = string(d);
assert_isequal(s(1), "00:00:01.000");
assert_isequal(ismissing(s), [false true]);
d.Format = 's';
s = string(d);
assert_isequal(s(1), "1 sec");
assert_isequal(ismissing(s), [false true]);
%=============================================================================
% Inf stays text, empty keeps its size
assert_isequal(string(seconds([Inf -Inf])), ["Inf sec", "-Inf sec"]);
assert_isequal(size(string(seconds(zeros(2, 0)))), [2 0]);
%=============================================================================
% singular unit for a magnitude of one
assert_isequal(string(days(-1)), "-1 day");
assert_isequal(string(days(2)), "2 days");
%=============================================================================
% display and char keep the NaN text
assert_isequal(char(seconds(NaN)), 'NaN sec');
R = evalc('disp(seconds([1 NaN]))');
assert_istrue(contains(R, 'NaN sec'));
%=============================================================================
% missing converts to a NaN duration
d = duration(missing);
assert_istrue(isnan(seconds(d)));
d = seconds([1 2]);
d(:) = missing;
assert_isequal(isnan(seconds(d)), [true true]);
d = [missing seconds(1)];
assert_isequal(d.Format, 's');
assert_isequal(cellstr(d), {'NaN sec', '1 sec'});
d = [missing hours(1) seconds(1)];
assert_isequal(d.Format, 'h');
d = [duration(1, 0, 0) seconds(1)];
assert_isequal(d.Format, 'hh:mm:ss');
d = seconds(1);
d(3) = missing;
assert_isequal(seconds(d(1:2)), [1 0]);
assert_istrue(isnan(seconds(d(3))));
%=============================================================================
