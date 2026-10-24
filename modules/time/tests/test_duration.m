%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
d = duration(1, 2, 3);
asserts.isequal(seconds(d), 3723);
d = duration('01:02:03');
asserts.isequal(seconds(d), 3723);
d = duration('1:02', 'InputFormat', 'mm:ss', 'Format', 'm');
asserts.isequal(seconds(d), 62);
d = duration([1 2 3; 4 5 6]);
asserts.isequal(seconds(d), [3723; 14706]);
d = hours([1 2 3; 4 5 6]);
asserts.isequal(numel(d), 6);
asserts.isequal(length(d), 3);
asserts.isequal(size(d), [2 3]);
[r, c] = size(d);
asserts.isequal([r c], [2 3]);
asserts.isequal(ndims(d), 2);
asserts.isequal(isrow(d), false);
asserts.isequal(iscolumn(d), false);
asserts.isequal(isvector(d), false);
asserts.isequal(hours(d(2, end)), 6);
%=============================================================================
% scalar multiplication and division (mtimes / mrdivide)
assert_isequal(seconds(hours(2) * 3), 6 * 3600);
assert_isequal(seconds(3 * hours(2)), 6 * 3600);
assert_isequal(hours(6) / hours(2), 3);
assert_isequal(seconds(hours(6) / 2), 3 * 3600);
%=============================================================================
% reduction and math overloads
d = hours([1 2 3 4 5]);
assert_isequal(hours(sum(d)), 15);
assert_isequal(hours(mean(d)), 3);
assert_isequal(hours(min(d)), 1);
assert_isequal(hours(max(d)), 5);
assert_isequal(hours(median(d)), 3);
assert_isequal(hours(cumsum(d)), [1 3 6 10 15]);
assert_isapprox(hours(std(d)), 1.5811388300841898, 1e-10);
assert_isequal(hours(abs(hours(-3))), 3);
assert_isequal(hours(max(hours(2), hours(5))), 5);
assert_isequal(minutes(round(minutes(90), 'hours')), 120);
assert_isequal(seconds(floor(seconds(2.7))), 2);
%=============================================================================
% mod and rem of two durations return a duration
assert_isequal(minutes(mod(minutes(90), minutes(60))), 30);
assert_isequal(hours(mod(hours(7), hours(3))), 1);
assert_isequal(minutes(rem(minutes(-90), minutes(60))), -30);
assert_isequal(class(mod(minutes(90), minutes(60))), 'duration');
assert_checkerror('mod(minutes(90), 5)', sprintf(_('%s of a duration requires both arguments to be durations.'), 'mod'));
%=============================================================================
