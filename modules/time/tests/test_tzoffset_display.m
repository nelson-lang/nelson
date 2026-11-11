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
t = datetime(2024, 3, 31, 2, 30, 0, 'TimeZone', 'Europe/Paris');
[d, dst] = tzoffset(t);
asserts.isequal(class(d), 'duration');
asserts.isequal(d.Format, 'hh:mm');
asserts.isequal(seconds(d), 7200);
asserts.isequal(dst.Format, 'hh:mm');
asserts.isequal(seconds(dst), 3600);
asserts.isequal(evalc('disp(d)'), sprintf('   02:00\n\n'));
txt = evalc('d');
asserts.istrue(contains(txt, '02:00'));
asserts.isfalse(contains(txt, 'sec'));
%=============================================================================
t = datetime(2024, [1 7], 1, 'TimeZone', 'America/New_York');
[d, dst] = tzoffset(t);
asserts.isequal(seconds(d), [-18000 -14400]);
asserts.isequal(seconds(dst), [0 3600]);
asserts.isequal(evalc('disp(d)'), sprintf('   -05:00   -04:00\n\n'));
asserts.isequal(evalc('disp(dst)'), sprintf('   00:00   01:00\n\n'));
%=============================================================================
d = tzoffset(datetime(2024, 1, 1, 'TimeZone', 'Asia/Kolkata'));
asserts.isequal(evalc('disp(d)'), sprintf('   05:30\n\n'));
%=============================================================================
[d, dst] = tzoffset(NaT('TimeZone', 'Europe/Paris'));
asserts.istrue(isnan(seconds(d)));
asserts.istrue(isnan(seconds(dst)));
asserts.isequal(d.Format, 'hh:mm');
%=============================================================================
[d, dst] = tzoffset(datetime(2024, 1, 1));
asserts.istrue(isnan(seconds(d)));
asserts.istrue(isnan(seconds(dst)));
asserts.isequal(evalc('disp(d)'), sprintf('   NaN\n\n'));
%=============================================================================
d = tzoffset(datetime.empty(0, 1));
asserts.isequal(size(d), [0 1]);
asserts.isequal(d.Format, 'hh:mm');
%=============================================================================
asserts.isequal(nargout('tzoffset'), 2);
%=============================================================================
% real daylight saving shift, not always one hour (Lord Howe style rule)
lordHowe = '<+1030>-10:30<+11>-11,M10.1.0,M4.1.0';
[tf, shift] = timezonedst(datenum(2024, [1 7], 1), lordHowe);
asserts.isequal(tf, [true false]);
asserts.isequal(shift, [1800 0]);
asserts.isequal(timezoneoffset(datenum(2024, [1 7], 1), lordHowe), [39600 37800]);
[tf, shift] = timezonedst(datenum(2024, [1 7], 1), 'Europe/Paris');
asserts.isequal(tf, [false true]);
asserts.isequal(shift, [0 3600]);
