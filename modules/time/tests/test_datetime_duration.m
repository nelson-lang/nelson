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
d = duration(1, 2, 3);
asserts.istrue(isduration(d));
asserts.isequal(seconds(d), 3723);
asserts.isequal(minutes(d), 62.05);
asserts.isapprox(hours(d), 3723 / 3600, 1e-12);
%=============================================================================
d = duration('01:02:03');
asserts.isequal(seconds(d), 3723);
d = duration('1:02', 'InputFormat', 'mm:ss');
asserts.isequal(seconds(d), 62);
%=============================================================================
d = [hours(1), minutes(30), seconds(45)];
asserts.isequal(seconds(d), [3600, 1800, 45]);
asserts.isequal(seconds(d(2)), 1800);
[h, m, s] = hms(duration(27, 5, 6));
asserts.isequal([h, m, s], [27, 5, 6]);
%=============================================================================
t = datetime(2024, 5, 17, 13, 14, 15);
asserts.istrue(isdatetime(t));
asserts.isequal([year(t), quarter(t), month(t), day(t), hour(t), minute(t), second(t)], ...
  [2024, 2, 5, 17, 13, 14, 15]);
%=============================================================================
t = datetime('2024-05-17', 'InputFormat', 'yyyy-MM-dd');
asserts.isequal([year(t), month(t), day(t)], [2024, 5, 17]);
asserts.isequal(yyyymmdd(t), 20240517);
%=============================================================================
t = [datetime(2024, 1, 1), datetime(2024, 1, 2)];
asserts.isequal(day(t), [1, 2]);
asserts.isequal(day(t(2)), 2);
%=============================================================================
t = datetime(2024, 1, 1):days(1):datetime(2024, 1, 3);
asserts.isequal(day(t), [1, 2, 3]);
d = hours(1):hours(1):hours(3);
asserts.isequal(hours(d), [1, 2, 3]);
%=============================================================================
t = [datetime(2024, 1, 2), NaT, datetime(2024, 1, 1)];
asserts.isequal(isnan(t), [false, true, false]);
asserts.isequal(isfinite(t), [true, false, true]);
[s, idx] = sort(t);
asserts.isequal(day(s), [1, 2, NaN]);
asserts.isequal(idx, [3, 1, 2]);
asserts.isequal(isequal(t, t), false);
asserts.isequal(isequaln(t, t), true);
%=============================================================================
d = [seconds(2), duration(NaN, 0, 0), seconds(1)];
asserts.isequal(isnan(d), [false, true, false]);
asserts.isequal(isfinite(d), [true, false, true]);
[sd, idx] = sort(d);
asserts.isequal(seconds(sd), [1, 2, NaN]);
asserts.isequal(idx, [3, 1, 2]);
asserts.isequal(isequal(d, d), false);
asserts.isequal(isequaln(d, d), true);
%=============================================================================
t = datetime(2024, 1, 31) + days(1);
asserts.isequal([year(t), month(t), day(t)], [2024, 2, 1]);
tv = datetime(2024, 1, 1) + days(0:2);
asserts.isequal(day(tv), [1 2 3]);
dt = datetime(2024, 1, 2) - datetime(2024, 1, 1);
asserts.isequal(seconds(dt), 86400);
%=============================================================================
t = datetime(1970, 1, 2);
asserts.isequal(posixtime(t), 86400);
asserts.isequal(juliandate(datetime(2000, 1, 1)), datenum(2000, 1, 1) + 1721058.5);
asserts.isequal(exceltime(datetime(1899, 12, 31)), 0);
%=============================================================================
n = NaT(2, 3);
asserts.isequal(size(datenum(n)), [2, 3]);
asserts.isequal(isnat(n), true(2, 3));
asserts.isequal(isnat(datetime(2024, 1, 1)), false);
%=============================================================================
t = datetime(2024, 5, 17, 13, 14, 15);
asserts.isequal(day(dateshift(t, 'start', 'month')), 1);
asserts.isequal(day(dateshift(t, 'end', 'month')), 31);
asserts.isequal(hour(dateshift(t, 'start', 'day')), 0);
f = dateshift(datetime(2024, 5, 17), 'dayofweek', 'Monday', 'next');
asserts.isequal(weekday(f), 2);
%=============================================================================
z = datetime(2024, 1, 1, 'TimeZone', '+02:30');
asserts.isequal(seconds(tzoffset(z)), 9000);
z = datetime(2024, 1, 1, 'TimeZone', 'Europe/Paris');
asserts.isequal(seconds(tzoffset(z)), 3600);
asserts.isequal(isdst(z), false);
z = datetime(2024, 7, 1, 'TimeZone', 'Europe/Paris');
asserts.isequal(seconds(tzoffset(z)), 7200);
asserts.isequal(isdst(z), true);
z = datetime(2024, 1, 1, 'TimeZone', 'America/New_York');
asserts.isequal(seconds(tzoffset(z)), -18000);
[zones, version] = timezones();
asserts.istrue(any(strcmp(cellstr(zones), 'Europe/Paris')));
asserts.istrue(any(strcmp(cellstr(zones), 'America/Los_Angeles')));
asserts.isequal(version, "nelson-minimal-2026");
asserts.isequal(isweekend(datetime(2024, 6, 8)), true);
%=============================================================================
