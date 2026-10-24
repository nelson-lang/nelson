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
t = datetime(2024, 5, 17, 13, 14, 15);
asserts.isequal([year(t), month(t), day(t), hour(t), minute(t), second(t)], [2024, 5, 17, 13, 14, 15]);
t = datetime('2024-05-17', 'InputFormat', 'yyyy-MM-dd', 'Format', 'yyyy-MM-dd');
asserts.isequal(yyyymmdd(t), 20240517);
%=============================================================================
% Delimiter-less InputFormat: each fixed-width numeric token consumes exactly
% its declared width instead of greedily eating the following token's digits.
t = datetime('20210315', 'InputFormat', 'yyyyMMdd');
asserts.isequal([year(t), month(t), day(t)], [2021, 3, 15]);
t = datetime('210315', 'InputFormat', 'yyMMdd');
asserts.isequal([year(t), month(t), day(t)], [2021, 3, 15]);
t = datetime('143045', 'InputFormat', 'HHmmss');
asserts.isequal([hour(t), minute(t), second(t)], [14, 30, 45]);
% Compact date and time joined by a quoted literal.
t = datetime('20210315T143045', 'InputFormat', 'yyyyMMdd''T''HHmmss');
asserts.isequal([year(t), month(t), day(t), hour(t), minute(t), second(t)], ...
  [2021, 3, 15, 14, 30, 45]);
% Compact day/month/year ordering.
t = datetime('15032021', 'InputFormat', 'ddMMyyyy');
asserts.isequal([year(t), month(t), day(t)], [2021, 3, 15]);
%=============================================================================
% Delimited InputFormat regression set (fixed-width change must not affect it).
t = datetime('17-May-2024', 'InputFormat', 'dd-MMM-yyyy');
asserts.isequal([year(t), month(t), day(t)], [2024, 5, 17]);
t = datetime('05/17/2024', 'InputFormat', 'MM/dd/yyyy');
asserts.isequal([year(t), month(t), day(t)], [2024, 5, 17]);
t = datetime('05/17/2024 14:30', 'InputFormat', 'MM/dd/yyyy HH:mm');
asserts.isequal([year(t), month(t), day(t), hour(t), minute(t)], [2024, 5, 17, 14, 30]);
t = datetime('2024-05-17 14:30:45', 'InputFormat', 'yyyy-MM-dd HH:mm:ss');
asserts.isequal([year(t), month(t), day(t), hour(t), minute(t), second(t)], ...
  [2024, 5, 17, 14, 30, 45]);
t = datetime([0 86400], 'ConvertFrom', 'posixtime');
asserts.isequal(day(t), [1 2]);
t = datetime(2024, [1 2], [3 4]);
asserts.isequal(month(t), [1 2]);
asserts.isequal(numel(t), 2);
asserts.isequal(length(t), 2);
asserts.isequal(size(t), [1 2]);
[r, c] = size(t);
asserts.isequal([r c], [1 2]);
asserts.isequal(ndims(t), 2);
asserts.isequal(isrow(t), true);
asserts.isequal(iscolumn(t), false);
asserts.isequal(isvector(t), true);
asserts.isequal(day(t(end)), 4);
%=============================================================================
% Colon with a calendarDuration step (variable-length calendar units).
fmt = @(d) datestr(d, 'yyyy-mm-dd');
r = datetime(2020, 1, 1):caldays(1):datetime(2020, 1, 5);
asserts.isequal(numel(r), 5);
asserts.isequal(fmt(r(1)), '2020-01-01');
asserts.isequal(fmt(r(end)), '2020-01-05');
% month-end preserved because each element is start + k*step (not cumulative)
r = datetime(2020, 1, 31):calmonths(1):datetime(2020, 5, 31);
asserts.isequal(numel(r), 5);
asserts.isequal(fmt(r(end)), '2020-05-31');
asserts.isequal(fmt(r(3)), '2020-03-31');
% descending / empty range
r = datetime(2020, 1, 5):caldays(1):datetime(2020, 1, 1);
asserts.isequal(numel(r), 0);
r = datetime(2020, 3, 1):calyears(1):datetime(2023, 3, 1);
asserts.isequal(numel(r), 4);
asserts.isequal(fmt(r(end)), '2023-03-01');
%=============================================================================
% min / max reductions
t = datetime(2023, 1, 1) + days([2 0 4 1 3]);
assert_isequal(min(t), datetime(2023, 1, 1));
assert_isequal(max(t), datetime(2023, 1, 5));
[m, i] = min(t);
assert_isequal(i, 2);
assert_isequal(min(datetime(2023, 1, 5), datetime(2023, 1, 3)), datetime(2023, 1, 3));
%=============================================================================
% mean / median reductions
t = datetime(2023, 1, 1) + days(0:4)';
assert_isequal(mean(t), datetime(2023, 1, 3));
assert_isequal(median(t), datetime(2023, 1, 3));
%=============================================================================
% Format property uses LDML tokens (MM = month, mm = minute)
d = datetime(2023, 6, 15, 10, 30, 45);
d.Format = 'yyyy-MM-dd';
assert_isequal(char(string(d)), '2023-06-15');
d.Format = 'yyyy-MM-dd HH:mm:ss';
assert_isequal(char(string(d)), '2023-06-15 10:30:45');
d.Format = 'dd/MM/yyyy';
assert_isequal(char(string(d)), '15/06/2023');
% string(datetime, format)
assert_isequal(char(string(datetime(2023, 6, 15), 'yyyy/MM/dd')), '2023/06/15');
%=============================================================================
% Format with month-name tokens (mmm/mmmm must not be corrupted by the
% month/minute swap)
d = datetime(2023, 6, 5, 9, 8, 7);
assert_isequal(char(string(d, 'MMM dd, yyyy')), 'Jun 05, 2023');
assert_isequal(char(string(d, 'MMMM dd, yyyy')), 'June 05, 2023');
assert_isequal(char(string(d, 'dd-MMM-yyyy')), '05-Jun-2023');
assert_isequal(char(string(d, 'dd MMM yyyy HH:mm')), '05 Jun 2023 09:08');
%=============================================================================
% LDML single 'd' is the day of month (it must not leak the weekday letter).
assert_isequal(char(string(datetime(2021, 3, 14, 'Format', 'MMMM d, yyyy'))), 'March 14, 2021');
%=============================================================================
% Full LDML token coverage: single tokens are unpadded, doubled tokens padded.
d = datetime(2021, 3, 14, 9, 5, 7);
assert_isequal(char(string(d, 'y')), '2021');
assert_isequal(char(string(d, 'yyyy')), '2021');
assert_isequal(char(string(d, 'M')), '3');
assert_isequal(char(string(d, 'MM')), '03');
assert_isequal(char(string(d, 'MMM')), 'Mar');
assert_isequal(char(string(d, 'MMMM')), 'March');
assert_isequal(char(string(d, 'd')), '14');
assert_isequal(char(string(d, 'dd')), '14');
assert_isequal(char(string(d, 'H')), '9');
assert_isequal(char(string(d, 'HH')), '09');
assert_isequal(char(string(d, 'm')), '5');
assert_isequal(char(string(d, 'mm')), '05');
assert_isequal(char(string(d, 's')), '7');
assert_isequal(char(string(d, 'ss')), '07');
assert_isequal(char(string(d, 'eee')), 'Sun');
assert_isequal(char(string(d, 'eeee')), 'Sunday');
assert_isequal(char(string(d, 'H:m:s')), '9:5:7');
assert_isequal(char(string(d, 'HH:mm:ss')), '09:05:07');
assert_isequal(char(string(d, 'yyyy/M/d')), '2021/3/14');
assert_isequal(char(string(d, 'eee, MMM d')), 'Sun, Mar 14');
% Single-digit components stay unpadded for single tokens, padded for doubles.
d2 = datetime(2021, 3, 4, 8, 5, 7);
assert_isequal(char(string(d2, 'd/M/y')), '4/3/2021');
assert_isequal(char(string(d2, 'dd/MM/yyyy')), '04/03/2021');
assert_isequal(char(string(d2, 'MMMM d, y')), 'March 4, 2021');
% Text inside single quotes is a literal.
assert_isequal(char(string(d, 'yyyy-MM-dd''T''HH:mm:ss')), '2021-03-14T09:05:07');
%=============================================================================
