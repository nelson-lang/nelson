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
% 'default' keeps the day of the month, 'endofmonth' / 'endofquarter' keep
% month / quarter ends
t = datetime(2000, 9, 30);
asserts.isequal(t + calendarDuration(0, 1, 0, 'ArithmeticMethod', 'default'), datetime(2000, 10, 30));
asserts.isequal(t + calendarDuration(0, 1, 0, 'ArithmeticMethod', 'endofmonth'), datetime(2000, 10, 31));
asserts.isequal(t + calendarDuration(0, 3, 0, 'ArithmeticMethod', 'endofquarter'), datetime(2000, 12, 31));
C = calquarters(0:3, 'ArithmeticMethod', 'endofquarter');
asserts.isequal(datetime(2000, 6, 30) + C, datetime(2000, [6 9 12 15], [30 30 31 31]));
M = calmonths(0:2, 'ArithmeticMethod', 'endofmonth');
asserts.isequal(datetime(2021, 2, 28) + M, datetime(2021, 2:4, [28 31 30]));
asserts.isequal(datetime(2021, 2, 28) + calmonths(0:2), datetime(2021, 2:4, 28));
asserts.isequal(datetime(2000, 1, 15) + calmonths(1, 'ArithmeticMethod', 'endofmonth'), datetime(2000, 2, 15));
asserts.isequal(datetime(2000, 1, 31) - calmonths(1, 'ArithmeticMethod', 'endofmonth'), datetime(1999, 12, 31));
%=============================================================================
L = calendarDuration(0, 3, 0, 'ArithmeticMethod', 'endofquarter');
asserts.isequal(L.ArithmeticMethod, 'endofquarter');
asserts.isequal(L.Format, 'qmdt');
asserts.isequal(calmonths(2, 'ArithmeticMethod', 'endofmonth').Format, 'mdt');
asserts.isequal(calmonths(2).ArithmeticMethod, 'default');
asserts.isequal(string(C), ["0q", "1q", "2q", "3q"]);
asserts.isequal(evalc('L'), sprintf('\nL = \n\n  calendarDuration (ArithmeticMethod="endofquarter")\n\n   1q\n\n'));
% the method follows indexing and concatenation of equal methods
asserts.isequal(C(2:3).ArithmeticMethod, 'endofquarter');
asserts.isequal([C, calquarters(4, 'ArithmeticMethod', 'endofquarter')].ArithmeticMethod, 'endofquarter');
asserts.isequal([C, calquarters(4)].ArithmeticMethod, 'default');
asserts.isequal((-C).ArithmeticMethod, 'endofquarter');
%=============================================================================
msg = 'With the arithmetic method ''%s'', the calendar duration must be whole %s without days or time.';
asserts.checkerror('calendarDuration(0, 1, 1, ''ArithmeticMethod'', ''endofmonth'')', ...
  sprintf(msg, 'endofmonth', 'months'), 'Nelson:time:arithmeticMethodWholeMonths');
asserts.checkerror('calmonths(1, ''ArithmeticMethod'', ''endofquarter'')', ...
  sprintf(msg, 'endofquarter', 'quarters'), 'Nelson:time:arithmeticMethodWholeMonths');
asserts.checkerror('calmonths(1, ''ArithmeticMethod'', ''bad'')', ...
  'The arithmetic method must be ''default'', ''endofmonth'' or ''endofquarter''.', ...
  'Nelson:time:invalidArithmeticMethod');
asserts.checkerror('L.ArithmeticMethod = ''default'';', ...
  'The ArithmeticMethod property is set when the calendar duration is created.', ...
  'Nelson:time:readOnlyArithmeticMethod');
%=============================================================================
% between, caldiff and isregular
b = between(datetime(2021, 2, 28), datetime(2021, 4, 30), 'months', 'ArithmeticMethod', 'endofmonth');
asserts.isequal(calmonths(b), 2);
asserts.isequal(b.ArithmeticMethod, 'endofmonth');
eom = {'ArithmeticMethod', 'endofmonth'};
asserts.isequal(calmonths(between(datetime(2021, 1, 31), datetime(2021, 4, 30), 'months', eom{:})), 3);
asserts.isequal(calmonths(between(datetime(2021, 4, 30), datetime(2021, 1, 31), 'months', eom{:})), -3);
b = between(datetime(2021, 3, 31), datetime(2021, 12, 31), 'quarters', 'ArithmeticMethod', 'endofquarter');
asserts.isequal(calquarters(b), 3);
asserts.checkerror('between(datetime(2021, 2, 28), datetime(2021, 4, 30), ''days'', eom{:})', ...
  'With the arithmetic method ''endofmonth'', the components must be ''months''.', ...
  'Nelson:time:arithmeticMethodComponents');
c = caldiff(datetime(2021, 1:3, [31 28 31]), 'months', 'ArithmeticMethod', 'endofmonth');
asserts.isequal(calmonths(c), [1 1]);
asserts.isequal(c.ArithmeticMethod, 'endofmonth');
asserts.istrue(isregular(datetime(2000, 1:3, [31 29 31]), 'endofmonth'));
asserts.isfalse(isregular(datetime(2000, 1:3, [31 28 31]), 'endofmonth'));
asserts.istrue(isregular(datetime(2000, [3 6 9], [31 30 30]), 'endofquarter'));
asserts.isfalse(isregular(datetime(2000, [3 6 10], [31 30 31]), 'endofquarter'));
TT = timetable(datetime(2000, 1:3, [31 29 31])', [1; 2; 3]);
asserts.istrue(isregular(TT, 'endofmonth'));
%=============================================================================
