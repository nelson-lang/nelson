%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[m, d, s] = split([calmonths(1), caldays(12)], {'months', 'days', 'time'});
assert_isequal(m, [1 0]);
assert_isequal(d, [0 12]);
assert_isequal(s, seconds([0 0]));
%=============================================================================
[y, m] = split(calmonths(14), {'years', 'months'});
assert_isequal(y, 1);
assert_isequal(m, 2);
%=============================================================================
[m, y] = split(calmonths(14), ["months", "years"]);
assert_isequal(y, 1);
assert_isequal(m, 2);
%=============================================================================
m = split(calmonths(14), {'months'});
assert_isequal(m, 14);
%=============================================================================
[q, m] = split(calmonths(-7), {'quarters', 'months'});
assert_isequal(q, -2);
assert_isequal(m, -1);
%=============================================================================
[w, d, t] = split(calendarDuration(0, 0, 17, 2, 0, 0), {'weeks', 'days', 'time'});
assert_isequal(w, 2);
assert_isequal(d, 3);
assert_isequal(t, hours(2));
%=============================================================================
[y, q, m, w, d] = split(calendarDuration(0, 29, 9), {'years', 'quarters', 'months', 'weeks', 'days'});
assert_isequal([y, q, m, w, d], [2 1 2 1 2]);
%=============================================================================
assert_checkerror('split(calmonths(1), {''months'', ''hours''})', 'Invalid calendar duration unit.');
%=============================================================================
assert_isequal(split(calmonths(14), 'months'), 14);
%=============================================================================
