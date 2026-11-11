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
zones = timezones();
asserts.istrue(any(strcmp(zones, 'Europe/Dublin')));
asserts.istrue(any(strcmp(zones, 'Africa/Casablanca')));
t = datetime(2024, 1, 1, 'TimeZone', 'Eire');
asserts.isequal(t.TimeZone, 'Europe/Dublin');
%=============================================================================
% Dublin: winter is standard time, summer a one hour daylight saving shift
v = [2024 1 1 12 0 0; 2024 7 1 12 0 0; 2024 3 31 0 30 0; 2024 3 31 2 30 0; 2024 10 27 1 30 0];
t = datetime(v, 'TimeZone', 'Europe/Dublin');
[d, dst] = tzoffset(t);
asserts.isequal(hours(d)', [0 1 0 1 0]);
asserts.isequal(hours(dst)', [0 1 0 1 0]);
asserts.isequal(isdst(t)', [false true false true false]);
asserts.isequal(evalc('disp(d(1:2)'')'), sprintf('   00:00   01:00\n\n'));
%=============================================================================
% Casablanca: +01 daylight saving time except during Ramadan
v = [2024 1 1 12 0 0; 2024 3 10 1 30 0; 2024 3 20 12 0 0; 2024 4 14 1 30 0; 2024 4 14 3 30 0; ...
     2025 2 23 12 0 0; 2087 6 1 12 0 0];
t = datetime(v, 'TimeZone', 'Africa/Casablanca');
[d, dst] = tzoffset(t);
asserts.isequal(hours(d)', [1 1 0 0 1 0 1]);
asserts.isequal(hours(dst)', [1 1 0 0 1 0 1]);
asserts.isequal(isdst(t)', [true true false false true false true]);
asserts.isequal(evalc('disp(d(2:3)'')'), sprintf('   01:00   00:00\n\n'));
%=============================================================================
% POSIX fallback rules
dn = datenum(2024, [1 7], 1, 12, 0, 0);
[~, shift] = timezonedst(dn, 'GMT0IST,M3.5.0/1,M10.5.0/2');
asserts.isequal(shift, [0 3600]);
asserts.isequal(timezoneoffset(dn, '<+01>-1'), [3600 3600]);
%=============================================================================
