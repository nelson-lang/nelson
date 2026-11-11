%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Europe/Paris goes back from 03:00 CEST to 02:00 CET on 27-Oct-2024
t0 = datetime(2024, 10, 27, 0, 30, 0, 'TimeZone', 'Europe/Paris');
t = t0 + hours(0:4);
asserts.isequal(t.Hour, [0 1 2 2 3]);
asserts.isequal(hours(tzoffset(t)), [2 2 2 1 1]);
asserts.isequal(isdst(t), [true true true false false]);
asserts.isequal(posixtime(t) - posixtime(t0), [0 3600 7200 10800 14400]);
%=============================================================================
% the two 02:30 are distinct instants
asserts.isfalse(t(3) == t(4));
asserts.istrue(t(3) < t(4));
asserts.isequal(seconds(t(4) - t(3)), 3600);
asserts.isfalse(isequal(t(3), t(4)));
asserts.isfalse(ismember(t(4), t(3)));
asserts.isequal(numel(unique([t(4) t(3) t(4)])), 2);
asserts.isequal(between(t(3), t(4)), calendarDuration(0, 0, 0, 1, 0, 0));
%=============================================================================
% elapsed arithmetic from either occurrence
asserts.isequal(hours(tzoffset(t(3) + minutes(30))), 1);
asserts.isequal((t(3) + minutes(30)).Hour, 2);
asserts.isequal(hours(tzoffset(t(4) - hours(1))), 2);
asserts.isequal(char(t(3) + days(1), 'yyyy-MM-dd HH:mm'), '2024-10-28 01:30');
x = t(3):minutes(30):t(5);
asserts.isequal(x.Minute, [30 0 30 0 30]);
asserts.isequal(hours(tzoffset(x)), [2 1 1 1 1]);
%=============================================================================
% the first occurrence survives indexing, reshaping, concatenation and sorting
asserts.isequal(hours(tzoffset([t(3); t(4)]')), [2 1]);
asserts.isequal(hours(tzoffset(reshape(t, 5, 1)))', [2 2 2 1 1]);
s = t;
s(1) = t(3);
asserts.isequal(hours(tzoffset(s)), [2 2 2 1 1]);
[cs, ix] = sort([t(4) t(3)]);
asserts.isequal(ix, [2 1]);
asserts.isequal(hours(tzoffset(cs)), [2 1]);
[mn, i] = min([t(4) t(3)]);
asserts.isequal(i, 2);
asserts.isequal(hours(tzoffset(mn)), 2);
[mx, i] = max([t(3) t(4)]);
asserts.isequal(i, 2);
asserts.isequal(hours(tzoffset(mx)), 1);
asserts.isequal(hours(tzoffset(mean([t(2) t(3)]))), 2);
%=============================================================================
% time zone changes keep the instant
u = t;
u.TimeZone = 'UTC';
asserts.isequal(u.Hour, [22 23 0 1 2]);
b = t(3);
b.TimeZone = 'Europe/Berlin';
asserts.isequal(hours(tzoffset(b)), 2);
y = datetime(2024, 10, 27, 1, 30, 0, 'TimeZone', 'UTC');
y.TimeZone = 'Europe/Paris';
asserts.isequal(hours(tzoffset(y)), 1);
z = datetime(2024, 10, 27, 0, 30, 0, 'TimeZone', 'UTC');
z.TimeZone = 'Europe/Paris';
asserts.isequal(hours(tzoffset(z)), 2);
r = t(3);
r.TimeZone = 'America/New_York';
asserts.isequal(r.Hour, 20);
%=============================================================================
% a new wall clock is resolved again: standard time
c = datetime(2024, 10, 27, 2, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(hours(tzoffset(c)), 1);
asserts.isequal(seconds(t(3) - c), -3600);
k = t(3);
k.Minute = 45;
asserts.isequal(hours(tzoffset(k)), 1);
k = t(3);
k.TimeZone = '';
k.TimeZone = 'Europe/Paris';
asserts.isequal(hours(tzoffset(k)), 1);
%=============================================================================
% calendar steps: days keep the source offset, months take standard time
asserts.isequal(hours(tzoffset(t(3) + caldays(0))), 2);
asserts.isequal(hours(tzoffset(t(3) + calmonths(0))), 2);
a = datetime(2024, 10, 26, 2, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(hours(tzoffset(a + caldays(1))), 2);
asserts.isequal(hours(tzoffset(caldays(1) + a)), 2);
d = datetime(2024, 10, 28, 2, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(hours(tzoffset(d - caldays(1))), 1);
e = datetime(2024, 9, 27, 2, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(hours(tzoffset(e + calmonths(1))), 1);
asserts.isequal(hours(tzoffset(t(3) + caldays(1) - caldays(1))), 1);
asserts.isequal(hours(tzoffset(t(3) + calendarDuration(0, 1, -31, 0, 0, 0))), 1);
f = datetime(2024, 10, 27, 1, 30, 0, 'TimeZone', 'Europe/Paris');
asserts.isequal(hours(tzoffset(f + calendarDuration(0, 0, 0, 1, 0, 0))), 2);
x = t(3):caldays(1):t(3) + caldays(2);
asserts.isequal(hours(tzoffset(x)), [2 1 1]);
%=============================================================================
% spring gap still moves forward
g = datetime(2024, 3, 30, 2, 30, 0, 'TimeZone', 'Europe/Paris') + caldays(1);
asserts.isequal(char(g, 'yyyy-MM-dd HH:mm'), '2024-03-31 03:30');
%=============================================================================
% dateshift keeps the occurrence
asserts.isequal(hours(tzoffset(dateshift(t(3), 'start', 'hour'))), 2);
asserts.isequal(hours(tzoffset(dateshift(t(4), 'start', 'hour'))), 1);
asserts.isequal(hours(tzoffset(dateshift(t(3), 'start', 'minute'))), 2);
%=============================================================================
% raw builtins: 'first' reads the first occurrence of an ambiguous time
dn = datenum(2024, 10, 27, 2, 30, 0);
asserts.isequal(timezoneoffset(dn, 'Europe/Paris'), 3600);
asserts.isequal(timezoneoffset(dn, 'Europe/Paris', '', 'first'), 7200);
[tf, shift] = timezonedst(dn, 'Europe/Paris', '', 'first');
asserts.isequal(tf, true);
asserts.isequal(shift, 3600);
[offset, repeated] = timezoneoffset([dn, dn + 1], 'Europe/Paris');
asserts.isequal(offset, [3600 3600]);
asserts.isequal(repeated, [true false]);
% UTC instants: 00:30 UTC is the first 02:30, 01:30 UTC the second one
utc = datenum(2024, 10, 27, [0 1], 30, 0);
[offset, repeated] = timezoneoffset(utc, 'Europe/Paris', '', 'utc');
asserts.isequal(offset, [7200 3600]);
asserts.isequal(repeated, [true false]);
%=============================================================================
