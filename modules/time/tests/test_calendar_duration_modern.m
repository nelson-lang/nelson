%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
c = calendarDuration(1, 2, 3);
asserts.istrue(iscalendarduration(c));
[y, m, d] = split(c);
asserts.isequal([y, m, d], [1, 2, 3]);
c = calendarDuration([1 2], [3 4], [5 6]);
[y, m, d] = split(c(2));
asserts.isequal([y, m, d], [2, 4, 6]);
%=============================================================================
t = datetime(2024, 1, 31) + calmonths(1);
asserts.isequal([year(t), month(t), day(t)], [2024, 2, 29]);
t = datetime(2024, 3, 31) - calmonths(1);
asserts.isequal([year(t), month(t), day(t)], [2024, 2, 29]);
%=============================================================================
c = calyears(1) + calquarters(2) + calmonths(3) + calweeks(1) + caldays(2);
[y, m, d] = split(c);
asserts.isequal([y, m, d], [1, 9, 9]);
%=============================================================================
c = [calmonths(1), calendarDuration(NaN, 0, 0)];
asserts.isequal(isnan(c), [false, true]);
asserts.isequal(isfinite(c), [true, false]);
asserts.isequal(c(1) == calmonths(1), true);
asserts.isequal(c(1) ~= calmonths(2), true);
asserts.isequal(isequal(c, c), false);
asserts.isequal(isequaln(c, c), true);
%=============================================================================
c = between(datetime(2024, 1, 15), datetime(2025, 3, 20));
[y, m, d] = split(c);
asserts.isequal([y, m, d], [1, 2, 5]);
%=============================================================================
t = [datetime(2024, 1, 1), datetime(2024, 2, 1), datetime(2024, 3, 1)];
c = caldiff(t);
asserts.istrue(iscalendarduration(c));
asserts.istrue(isregular(t, 'months'));
%=============================================================================
asserts.isequal(isbetween(datetime(2024, 1, 2), datetime(2024, 1, 1), datetime(2024, 1, 3)), true);
asserts.isequal(isbetween(datetime(2024, 1, 1), datetime(2024, 1, 1), datetime(2024, 1, 3), 'open'), false);
asserts.isequal(isdst(datetime(2024, 1, 1)), false);
%=============================================================================
asserts.isequal(weeknum(datetime(2024, 1, 8)), 2);
asserts.isequal(months(datetime(2024, 1, 15), datetime(2024, 3, 16)), 2);
asserts.isequal(eomdate(2024, 2), datenum(2024, 2, 29));
%=============================================================================
asserts.isequal(nweekdate(2, 2, 2024, 5), datenum(2024, 5, 13));
asserts.isequal(lweekdate(2, 2024, 5), datenum(2024, 5, 27));
%=============================================================================
x = m2xdate(datetime(2024, 1, 1));
asserts.isequal(x2mdate(x), datenum(2024, 1, 1));
asserts.istrue(isdatetime(x2mdate(x, 'datetime')));
%=============================================================================
