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
asserts.istrue(any(strcmp(zones, 'Australia/Lord_Howe')));
asserts.istrue(any(strcmp(zones, 'Antarctica/Troll')));
t = datetime(2024, 1, 1, 'TimeZone', 'Australia/LHI');
asserts.isequal(t.TimeZone, 'Australia/Lord_Howe');
%=============================================================================
% Lord Howe: 30 minute daylight saving shift
v = [2024 1 1 12 0 0; 2024 7 1 12 0 0; 2024 10 6 1 59 0; 2024 10 6 2 15 0; ...
     2024 4 7 1 15 0; 2024 4 7 1 45 0];
t = datetime(v, 'TimeZone', 'Australia/Lord_Howe');
[d, dst] = tzoffset(t);
asserts.isequal(minutes(d)', [660 630 630 660 660 630]);
asserts.isequal(minutes(dst)', [30 0 0 30 30 0]);
asserts.isequal(isdst(t)', [true false false true true false]);
% the time skipped by the gap moves forward
asserts.isequal(char(t(4), 'yyyy-MM-dd HH:mm'), '2024-10-06 02:45');
asserts.isequal(evalc('disp(d(1:2)'')'), sprintf('   11:00   10:30\n\n'));
asserts.isequal(evalc('disp(dst(1:2)'')'), sprintf('   00:30   00:00\n\n'));
%=============================================================================
% Troll: 2 hour daylight saving shift
v = [2024 1 1 12 0 0; 2024 7 1 12 0 0; 2024 3 31 0 59 0; 2024 3 31 3 0 0; 2024 10 27 3 30 0];
t = datetime(v, 'TimeZone', 'Antarctica/Troll');
[d, dst] = tzoffset(t);
asserts.isequal(hours(d)', [0 2 0 2 0]);
asserts.isequal(hours(dst)', [0 2 0 2 0]);
asserts.isequal(evalc('disp(dst(1:2)'')'), sprintf('   00:00   02:00\n\n'));
%=============================================================================
% POSIX fallback rules give the same shifts
dn = datenum(2024, [1 7], 1, 12, 0, 0);
[~, shift] = timezonedst(dn, '<+00>0<+02>-2,M3.5.0/1,M10.5.0/3');
asserts.isequal(shift, [0 7200]);
[~, shift] = timezonedst(dn, '<+1030>-10:30<+11>-11,M10.1.0,M4.1.0');
asserts.isequal(shift, [1800 0]);
%=============================================================================
