%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
c = between(datetime(2024, 1, 15), datetime(2024, 3, 20));
asserts.isequal(split(c, 'months'), 2);
asserts.isequal(split(c, 'days'), 5);
c = between(datetime(2024, 1, 1), datetime(2025, 1, 1), 'years');
asserts.isequal(split(c, 'years'), 1);
c = between(datetime(2024, 1, 1, 1, 0, 0), datetime(2024, 1, 1, 2, 30, 0), 'days');
% only the requested components: no time part
asserts.isequal(seconds(time(c)), 0);
asserts.isequal(char(c), '0d');
%=============================================================================
% a day is borrowed when the time of day of t2 is earlier
a = datetime(2024, 1, 31, 12, 0, 0);
b = datetime(2024, 4, 1, 6, 0, 0);
c = between(a, b);
asserts.isequal(char(c), '2mo 18h 0m 0s');
asserts.isequal(c.Format, 'ymdt');
asserts.isequal(char(between(b, a)), '-2mo -18h 0m 0s');
asserts.isequal(char(between(a, b, 'days')), '60d');
asserts.isequal(between(a, b, 'days').Format, 'mdt');
asserts.isequal(char(between(a, b, 'Days')), '60d');
asserts.isequal(char(between(a, b, 'months')), '2mo');
asserts.isequal(char(between(a, b, 'weeks')), '8w');
asserts.isequal(between(a, b, 'weeks').Format, 'mwdt');
asserts.isequal(char(between(a, b, {'weeks', 'days'})), '8w 4d');
asserts.isequal(char(between(a, b, 'time')), '1458h 0m 0s');
asserts.isequal(char(between(a, b, {'days', 'time'})), '60d 18h 0m 0s');
asserts.isequal(char(between(a, b, 'years')), '0d');
asserts.isequal(char(between(b, a, {'weeks', 'days'})), '-8w -4d');
%=============================================================================
c1 = datetime(2024, 1, 1, 12, 0, 0);
c2 = datetime(2024, 1, 3, 6, 0, 0);
asserts.isequal(char(between(c1, c2)), '1d 18h 0m 0s');
asserts.isequal(char(between(c2, c1)), '-1d -18h 0m 0s');
asserts.isequal(char(between(c1, c2, 'days')), '1d');
asserts.isequal(char(between(datetime(2024, 1, 1, 10, 0, 0), datetime(2024, 1, 1, 8, 0, 0))), '-2h 0m 0s');
asserts.isequal(char(between(datetime(2024, 5, 31, 10, 0, 0), datetime(2024, 6, 30, 9, 0, 0))), '29d 23h 0m 0s');
%=============================================================================
% month ends clamp
asserts.isequal(char(between(datetime(2024, 1, 31), datetime(2024, 2, 29))), '1mo');
asserts.isequal(char(between(datetime(2024, 1, 31), datetime(2024, 2, 29), 'days')), '29d');
asserts.isequal(char(between(datetime(2024, 3, 31), datetime(2024, 2, 29))), '-1mo');
asserts.isequal(char(between(datetime(2024, 3, 31), datetime(2024, 2, 29), 'days')), '-31d');
%=============================================================================
y1 = datetime(2023, 2, 28, 23, 0, 0);
y2 = datetime(2024, 2, 29, 1, 0, 0);
asserts.isequal(char(between(y1, y2)), '1y 2h 0m 0s');
asserts.isequal(char(between(y1, y2, 'months')), '12mo');
asserts.isequal(char(between(y1, y2, 'quarters')), '4q');
asserts.isequal(between(y1, y2, 'quarters').Format, 'qmdt');
asserts.isequal(char(between(y1, y2, 'weeks')), '52w');
asserts.isequal(char(between(y1, y2, {'weeks', 'days'})), '52w 1d');
asserts.isequal(char(between(y1, y2, {'quarters', 'time'})), '4q 2h 0m 0s');
%=============================================================================
asserts.isequal(char(between(datetime(2024, 1, 1), datetime(2024, 1, 1))), '0d');
asserts.isequal(char(between(NaT, datetime(2024, 1, 1))), 'NaN');
c = between(datetime(2024, 1, 1), datetime(2024, [2 3], 1));
asserts.isequal(split(c, 'months'), [1 2]);
%=============================================================================
