%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% setting TimeZone: a zoned datetime keeps its instants
t = datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'UTC');
t.TimeZone = 'Europe/Paris';
assert_isequal(t.Hour, 13);
assert_isequal(t.TimeZone, 'Europe/Paris');
assert_isequal(posixtime(t), 1577880000);
t = datetime(2020, 7, 1, 12, 0, 0, 'TimeZone', 'UTC');
t.TimeZone = 'America/New_York';
assert_isequal(t.Hour, 8);
t = datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'UTC');
t.TimeZone = '+05:30';
assert_isequal([t.Hour, t.Minute], [17, 30]);
t = datetime(2020, [1, 7], 1, 12, 0, 0, 'TimeZone', 'UTC');
t.TimeZone = 'Europe/Paris';
assert_isequal(t.Hour, [13, 14]);
t = [NaT('TimeZone', 'UTC'), datetime(2020, 1, 1, 'TimeZone', 'UTC')];
t.TimeZone = 'Asia/Tokyo';
assert_istrue(isnat(t(1)));
assert_isequal(t(2).Hour, 9);
% an unzoned datetime keeps its wall clock, and so does removing the zone
t = datetime(2020, 1, 1, 12, 0, 0);
t.TimeZone = 'Europe/Paris';
assert_isequal(t.Hour, 12);
t.TimeZone = '';
assert_isequal(t.Hour, 12);
assert_isequal(t.TimeZone, '');
%=============================================================================
% constructor with a datetime input and a TimeZone
u = datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'UTC');
assert_isequal(datetime(u, 'TimeZone', 'Asia/Tokyo').Hour, 21);
assert_isequal(datetime(datetime(2020, 1, 1, 12, 0, 0), 'TimeZone', 'Asia/Tokyo').Hour, 12);
assert_isequal(datetime(u, 'TimeZone', '').TimeZone, '');
assert_isequal(datetime(u, 'TimeZone', '').Hour, 12);
assert_isequal(datetime(u, 'Format', 'yyyy').TimeZone, 'UTC');
%=============================================================================
% numeric inputs: posixtime and juliandate count UTC instants
assert_isequal(datetime(1577880000, 'ConvertFrom', 'posixtime', 'TimeZone', 'Europe/Paris').Hour, 13);
assert_isequal(datetime(1577880000, 'ConvertFrom', 'posixtime').Hour, 12);
assert_isequal(datetime(2458850, 'ConvertFrom', 'juliandate', 'TimeZone', 'Europe/Paris').Hour, 13);
assert_isequal(datetime(737791.5, 'ConvertFrom', 'datenum', 'TimeZone', 'Europe/Paris').Hour, 12);
assert_isequal(datetime(43831.5, 'ConvertFrom', 'excel', 'TimeZone', 'Europe/Paris').Hour, 12);
t = datetime(3600, 'ConvertFrom', 'epochtime', 'Epoch', datetime(2020, 1, 1), 'TimeZone', 'Europe/Paris');
assert_isequal(t.Hour, 1);
t = datetime(3600, 'ConvertFrom', 'epochtime', 'Epoch', datetime(2020, 1, 1, 'TimeZone', 'UTC'), 'TimeZone', 'Europe/Paris');
assert_isequal(t.Hour, 2);
p = datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'Europe/Paris');
assert_isequal(posixtime(p), 1577876400);
assert_isapprox(juliandate(p), 2458849 + 23 / 24, 1e-9);
assert_isequal(juliandate(datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'UTC')), 2458850);
assert_isequal(juliandate(datetime(2020, 1, 1, 12, 0, 0)), 2458850);
assert_isequal(datenum(p) - 737791, 0.5);
%=============================================================================
% time zone names
t = datetime(2020, 1, 1, 'TimeZone', 'local');
if ~iswasm()
  assert_isfalse(strcmpi(t.TimeZone, 'local'));
end
assert_isfalse(isempty(t.TimeZone));
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'GMT').TimeZone, 'UTC');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'utc').TimeZone, 'UTC');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'Z').TimeZone, '+00:00');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', '+0530').TimeZone, '+05:30');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', hours(-3)).TimeZone, '-03:00');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', '+5').TimeZone, '+05:00');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'GMT-3:30').TimeZone, '-03:30');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'utc+5').TimeZone, '+05:00');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'GMT+0530').TimeZone, '+05:30');
% aliases name their zone; 'Etc/' zones are fixed offsets (Etc/GMT+5 is UTC-5)
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'US/Eastern').TimeZone, 'America/New_York');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'Japan').TimeZone, 'Asia/Tokyo');
t = datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'UTC');
t.TimeZone = 'Etc/GMT+5';
assert_isequal(t.TimeZone, '-05:00');
assert_isequal(t.Hour, 7);
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'Etc/UTC').TimeZone, '+00:00');
assert_isequal(datetime(2020, 1, 1, 'TimeZone', 'Etc/GMT-14').TimeZone, '+14:00');
msg = 'Unable to create the specified time zone: ''Etc/GMT+15''. See the datetime.TimeZone property for details about specifying time zones.';
assert_checkerror('datetime(2020, 1, 1, ''TimeZone'', ''Etc/GMT+15'')', msg, 'Nelson:datetime:UnknownTimeZone');
msg = 'Unable to create the specified time zone: ''Foo/Bar''. See the datetime.TimeZone property for details about specifying time zones.';
assert_checkerror('datetime(2020, 1, 1, ''TimeZone'', ''Foo/Bar'')', msg, 'Nelson:datetime:UnknownTimeZone');
t = datetime(2020, 1, 1);
assert_checkerror('t.TimeZone = ''Foo/Bar'';', msg, 'Nelson:datetime:UnknownTimeZone');
msg = 'Unable to create the specified time zone: ''europe/paris''. See the datetime.TimeZone property for details about specifying time zones.';
assert_checkerror('datetime(2020, 1, 1, ''TimeZone'', ''europe/paris'')', msg, 'Nelson:datetime:UnknownTimeZone');
msg = 'Unable to create the specified time zone: ''+25:00''. See the datetime.TimeZone property for details about specifying time zones.';
assert_checkerror('datetime(2020, 1, 1, ''TimeZone'', ''+25:00'')', msg, 'Nelson:datetime:UnknownTimeZone');
msg = 'A time zone must be specified as a string scalar, non-NaN duration scalar, or character vector.';
assert_checkerror('t.TimeZone = 5;', msg, 'Nelson:datetime:InvalidTimeZone');
%=============================================================================
% zoned and unzoned datetimes do not combine
u = datetime(2020, 1, 1, 12, 0, 0, 'TimeZone', 'UTC');
n = datetime(2020, 1, 1, 12, 0, 0);
p = datetime(2020, 1, 1, 13, 0, 0, 'TimeZone', 'Europe/Paris');
msg = 'Cannot combine or compare a datetime array with a time zone with one without a time zone.';
id = 'Nelson:datetime:IncompatibleTZ';
assert_checkerror('u < n', msg, id);
assert_checkerror('u == n', msg, id);
assert_checkerror('n >= u', msg, id);
assert_checkerror('u - n', msg, id);
assert_checkerror('n - u', msg, id);
assert_checkerror('max(u, n)', msg, id);
assert_checkerror('ismember(u, n)', msg, id);
assert_checkerror('isbetween(u, n, n)', msg, id);
assert_checkerror('u(2) = n;', msg, id);
assert_checkerror('n(2) = u;', msg, id);
msg = 'Unable to concatenate a datetime array that has a time zone with one that does not have a time zone.';
id = 'Nelson:datetime:cat:IncompatibleTZ';
assert_checkerror('[u, n]', msg, id);
assert_checkerror('[n, u]', msg, id);
assert_checkerror('[u, NaT]', msg, id);
assert_checkerror('cat(1, u, n)', msg, id);
assert_isfalse(isequal(u, n));
assert_isfalse(u == missing);
%=============================================================================
% two time zones combine by instant, in the time zone of the first operand
assert_istrue(isequal(u, p));
assert_istrue(isequaln(u, p));
assert_istrue(u == p);
assert_istrue(ismember(u, p));
assert_istrue(isbetween(u, p - minutes(30), p + minutes(30)));
c = [u, p];
assert_isequal(c.TimeZone, 'UTC');
assert_isequal(c.Hour, [12, 12]);
c = [p, u];
assert_isequal(c.TimeZone, 'Europe/Paris');
assert_isequal(c.Hour, [13, 13]);
c = [u, "2020-01-02"];
assert_isequal(c.TimeZone, 'UTC');
assert_isequal(c(2).Hour, 0);
assert_isequal(min(u, p).TimeZone, 'UTC');
assert_isequal(numel(union(u, p)), 1);
a = u;
a(2) = p;
assert_isequal(a.Hour, [12, 12]);
a(3) = '2020-01-05 00:00:00';
assert_isequal(a(3).Day, 5);
r = u:hours(1):datetime(2020, 1, 1, 15, 0, 0, 'TimeZone', 'Europe/Paris');
assert_isequal(r.Hour, [12, 13, 14]);
assert_isequal(r.TimeZone, 'UTC');
%=============================================================================
% DST: a skipped time moves after the gap, an ambiguous time is standard time
t = datetime(2020, 3, 29, 2, 30, 0, 'TimeZone', 'Europe/Paris');
assert_isequal(t.Hour, 3);
assert_isequal(posixtime(t) - 1585440000, 5400);
t = datetime(2020, 3, 29, 2, 30, 0);
t.TimeZone = 'Europe/Paris';
assert_isequal(t.Hour, 3);
t = datetime(2020, 3, 29, 1, 0, 0, 'TimeZone', 'Europe/Paris');
t.Hour = 2;
assert_isequal(t.Hour, 3);
t = datetime(2020, 10, 25, 2, 30, 0, 'TimeZone', 'Europe/Paris');
assert_isequal(posixtime(t) - 1603584000, 5400);
assert_isequal(tzoffset(t), hours(1));
assert_isfalse(isdst(t));
t = datetime(2020, 11, 1, 1, 30, 0, 'TimeZone', 'America/New_York');
assert_isequal(posixtime(t) - 1604188800, 23400);
% fixed-length durations and numeric days count elapsed time
t = datetime(2020, 3, 28, 12, 0, 0, 'TimeZone', 'Europe/Paris');
assert_isequal((t + days(1)).Hour, 13);
assert_isequal((t + hours(24)).Hour, 13);
assert_isequal((t + 1).Hour, 13);
assert_isequal((t + caldays(1)).Hour, 12);
assert_isequal((datetime(2020, 3, 29, 13, 0, 0, 'TimeZone', 'Europe/Paris') - hours(24)).Hour, 12);
assert_isequal(datetime(2020, 3, 29, 12, 0, 0, 'TimeZone', 'Europe/Paris') - t, hours(23));
r = datetime(2020, 3, 29, 1, 0, 0, 'TimeZone', 'Europe/Paris'):hours(1):datetime(2020, 3, 29, 4, 0, 0, 'TimeZone', 'Europe/Paris');
assert_isequal(r.Hour, [1, 3, 4]);
assert_isequal(datetime(2020, 10, 25, 2, 30, 0, 'TimeZone', 'Europe/Paris') - datetime(2020, 10, 25, 0, 0, 0, 'TimeZone', 'UTC'), minutes(90));
%=============================================================================
% unzoned arithmetic is unchanged
n = datetime(2020, 3, 28, 12, 0, 0);
assert_isequal((n + days(1)).Hour, 12);
assert_isequal((n + 1).Hour, 12);
r = datetime(2020, 1, 1):days(1):datetime(2020, 1, 3);
assert_isequal(r.Day, [1, 2, 3]);
%=============================================================================
