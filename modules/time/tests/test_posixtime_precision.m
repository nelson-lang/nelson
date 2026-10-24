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
% Sub-second precision of datetime instants far from the serial-day origin.
% Folding the time of day into the large day count used to lose a few
% microseconds; posixtime/second must stay exact.
%=============================================================================
% One second past the epoch is exactly one, not 1.0000046.
asserts.istrue(posixtime(datetime(1970, 1, 1, 0, 0, 1, 'TimeZone', 'UTC')) == 1);
asserts.istrue(posixtime(datetime(1970, 1, 1, 0, 0, 0, 'TimeZone', 'UTC')) == 0);
asserts.istrue(posixtime(datetime(1970, 1, 2, 'TimeZone', 'UTC')) == 86400);
%=============================================================================
% Modern dates, whole seconds.
asserts.istrue(posixtime(datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', 'UTC')) == 1577836800);
asserts.istrue(posixtime(datetime(2038, 1, 19, 3, 14, 7, 'TimeZone', 'UTC')) == 2147483647);
%=============================================================================
% Far future, whole seconds.
asserts.istrue(posixtime(datetime(3000, 6, 15, 12, 30, 45, 'TimeZone', 'UTC')) == 32517981045);
%=============================================================================
% Sub-second fractions that are exactly representable.
asserts.istrue(posixtime(datetime(2020, 3, 15, 10, 20, 30.5, 'TimeZone', 'UTC')) == 1584267630.5);
asserts.istrue(posixtime(datetime(2020, 3, 15, 10, 20, 30.001, 'TimeZone', 'UTC')) == 1584267630.001);
%=============================================================================
% Microsecond fraction: exact well below a microsecond.
asserts.isapprox(posixtime(datetime(2020, 3, 15, 10, 20, 30.123456, 'TimeZone', 'UTC')), ...
  1584267630.123456, 1e-6);
%=============================================================================
% second() reads the seconds (fraction included) exactly.
asserts.istrue(second(datetime(1970, 1, 1, 0, 0, 1)) == 1);
asserts.istrue(second(datetime(2020, 3, 15, 10, 20, 30.5)) == 30.5);
asserts.isapprox(second(datetime(2020, 3, 15, 10, 20, 30.123456)), 30.123456, 1e-6);
%=============================================================================
% Vector construction keeps every element exact.
t = datetime([1970 2020; 2000 3000], [1 3; 6 6], [1 15; 15 15], ...
  [0 10; 12 12], [0 20; 0 30], [1 30.5; 0 45], 'TimeZone', 'UTC');
asserts.isequal(posixtime(t), [1 1584267630.5; 961070400 32517981045]);
%=============================================================================
% Concatenation of separately built instants stays exact.
a = datetime(1970, 1, 1, 0, 0, 1, 'TimeZone', 'UTC');
b = datetime(2020, 3, 15, 10, 20, 30.5, 'TimeZone', 'UTC');
asserts.isequal(posixtime([a, b]), [1 1584267630.5]);
%=============================================================================
% Indexing an exact array preserves the precise instant.
asserts.istrue(posixtime(t(1)) == 1);
asserts.istrue(posixtime(t(1, 2)) == 1584267630.5);
%=============================================================================
% Adding a duration keeps the instant exact.
asserts.istrue(posixtime(datetime(1970, 1, 1, 0, 0, 0, 'TimeZone', 'UTC') + seconds(1)) == 1);
asserts.istrue(posixtime(datetime(2020, 1, 1, 0, 0, 0, 'TimeZone', 'UTC') + seconds(0.5)) == 1577836800.5);
%=============================================================================
% Difference between two instants is exact.
d1 = datetime(2020, 3, 15, 10, 20, 30.5, 'TimeZone', 'UTC');
d2 = datetime(2020, 3, 15, 10, 20, 29.25, 'TimeZone', 'UTC');
asserts.istrue(seconds(d1 - d2) == 1.25);
%=============================================================================
% Round-trip through the posixtime conversion is exact.
asserts.istrue(posixtime(datetime(1.5, 'ConvertFrom', 'posixtime', 'TimeZone', 'UTC')) == 1.5);
asserts.istrue(posixtime(datetime(1577836800, 'ConvertFrom', 'posixtime', 'TimeZone', 'UTC')) == 1577836800);
%=============================================================================
% NaT propagates to NaN.
asserts.istrue(isnan(posixtime(datetime(NaN, 'ConvertFrom', 'posixtime'))));
%=============================================================================
% Setting a date field keeps the seconds fraction exact.
tset = datetime(2020, 3, 15, 10, 20, 30.123456);
tset.Year = 2021;
asserts.isapprox(second(tset), 30.123456, 1e-6);
%=============================================================================
% datenum of a datetime is unchanged for whole dates.
asserts.isequal(datenum(datetime(2020, 1, 1)), datenum(2020, 1, 1));
asserts.isequal(datenum(datetime(1970, 1, 1)), datenum(1970, 1, 1));
%=============================================================================
