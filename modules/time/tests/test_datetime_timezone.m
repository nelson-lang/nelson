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
% A named IANA zone applies its UTC offset when the zoned wall-clock time is
% mapped to the absolute instant used for differences and comparisons.
d1 = datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', 'America/New_York');
d2 = datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', 'UTC');
asserts.isequal(hours(d2 - d1), -5);
asserts.istrue(d2 < d1);
asserts.isfalse(d1 < d2);
asserts.istrue(d1 > d2);
asserts.isfalse(d1 == d2);
%=============================================================================
% Daylight saving time is taken from the bundled time zone data (July -> EDT).
s1 = datetime(2020, 7, 1, 0, 0, 0, 'TimeZone', 'America/New_York');
s2 = datetime(2020, 7, 1, 0, 0, 0, 'TimeZone', 'UTC');
asserts.isequal(hours(s2 - s1), -4);
%=============================================================================
% Fixed '+HH:MM' / '-HH:MM' offsets.
a = datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', '+05:00');
b = datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', 'UTC');
asserts.isequal(hours(b - a), 5);
c = datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', '-03:00');
asserts.isequal(hours(b - c), -3);
%=============================================================================
% posixtime applies the zone offset (unzoned datetimes are treated as UTC).
asserts.isequal(posixtime(d2), 1577836800);
asserts.isequal(posixtime(d1), 1577836800 + 5 * 3600);
asserts.isequal(posixtime(datetime(2020, 1, 1, 0, 0, 0)), 1577836800);
%=============================================================================
% The same absolute instant expressed in two zones compares equal.
p = datetime(2020, 1, 1, 5, 0, 0, 'TimeZone', 'UTC');
q = datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', 'America/New_York');
asserts.istrue(p == q);
asserts.isequal(seconds(p - q), 0);
%=============================================================================
% Unzoned comparisons and differences are unchanged by the offset handling.
asserts.istrue(datetime(2020, 1, 1) < datetime(2020, 1, 2));
asserts.isequal(hours(datetime(2020, 1, 1, 12, 0, 0) - datetime(2020, 1, 1, 0, 0, 0)), 12);
%=============================================================================
