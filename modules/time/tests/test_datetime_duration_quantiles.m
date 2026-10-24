%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% duration: prctile, quantile and iqr return duration values
d = hours([1 2 3 4 10]);
p = prctile(d, [25 50 75]);
assert_isequal(class(p), 'duration');
assert_isequal(hours(p), [1.75 3 5.5]);
q = quantile(d, 0.5);
assert_isequal(class(q), 'duration');
assert_isequal(hours(q), 3);
[r, q] = iqr(d);
assert_isequal(class(r), 'duration');
assert_isequal(class(q), 'duration');
assert_isequal(hours(r), 3.75);
assert_isequal(hours(q), [1.75 5.5]);
assert_isequal(hours(iqr(d)), 3.75);
%=============================================================================
% duration: NaN removed, dim, 'all', Format kept
d = hours([1 NaN 3; 4 5 6]);
assert_isequal(hours(prctile(d, 50)), [2.5 5 4.5]);
assert_isequal(hours(prctile(d, 50, 2)), [2; 5]);
assert_isequal(hours(iqr(d, 'all')), 2.75);
d = minutes([1 2 3 4]);
d.Format = 'm';
assert_isequal(prctile(d, 50).Format, 'm');
assert_isequal(iqr(d).Format, 'm');
%=============================================================================
% datetime: prctile and quantile return datetime values
t = datetime(2024, 1, [1 3 5 7 30]);
P = prctile(t, [25 50 75]);
assert_isequal(class(P), 'datetime');
assert_isequal(datenum(P), datenum(2024, 1, 1) + [1.5 4 11.75]);
Q = quantile(t, [0.25 0.5], 'Method', 'inclusive');
assert_isequal(class(Q), 'datetime');
assert_isequal(datenum(Q), datenum(2024, 1, [3 5]));
t.Format = 'yyyy-MM-dd';
assert_isequal(prctile(t, 50).Format, 'yyyy-MM-dd');
%=============================================================================
% datetime: iqr returns a duration, the quartiles are datetime values
t = datetime(2024, 1, [1 3 5 7 30]);
[R, Q] = iqr(t);
assert_isequal(class(R), 'duration');
assert_isequal(class(Q), 'datetime');
assert_isapprox(hours(R), 246, 1e-6);
assert_isequal(datenum(Q), datenum(2024, 1, 1) + [1.5 11.75]);
%=============================================================================
% datetime: NaT removed, dim and 'all'
t = datetime(2024, 1, [1 2 3]);
t(2) = NaT;
assert_isequal(datenum(prctile(t, 50)), datenum(2024, 1, 2));
T = reshape(datetime(2024, 1, 1:6), 2, 3);
assert_isequal(datenum(prctile(T, 50, 2)), datenum(2024, 1, [3; 4]));
assert_isapprox(days(iqr(T, 'all')), 3, 1e-9);
assert_isequal(size(quantile(T, [0.25 0.75], [1 2])), [2 1]);
%=============================================================================
