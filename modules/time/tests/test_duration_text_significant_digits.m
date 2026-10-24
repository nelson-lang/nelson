%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% The number in a duration element text keeps five significant digits;
% the exponent of the unrounded value chooses between fixed and exponent
% notation.
%=============================================================================
values = [99999, 100000, 123456, 1e10, 1e-5, 0.1234567, -123456, ...
  -0.1234567, 12345.678, 1.5, -300, Inf, -Inf, 0, -0, 1, 99999.5, ...
  -99999.5, 99999.4, 999995, 1e15, 123.45678, 0.00012345, 0.0000999996, ...
  9999.95, 0.99999999, 172800, 120000];
expected = ["99999 sec", "1e+05 sec", "1.2346e+05 sec", "1e+10 sec", ...
  "1e-05 sec", "0.12346 sec", "-1.2346e+05 sec", "-0.12346 sec", ...
  "12346 sec", "1.5 sec", "-300 sec", "Inf sec", "-Inf sec", "0 sec", ...
  "0 sec", "1 sec", "100000 sec", "-100000 sec", "99999 sec", ...
  "1e+06 sec", "1e+15 sec", "123.46 sec", "0.00012345 sec", "1e-04 sec", ...
  "10000 sec", "1 sec", "1.728e+05 sec", "1.2e+05 sec"];
assert_isequal(string(seconds(values)), expected);
%=============================================================================
d = seconds(123456000);
d.Format = 'm';
assert_isequal(string(d), "2.0576e+06 min");
d.Format = 'h';
assert_isequal(string(d), "34293 hr");
d.Format = 'd';
assert_isequal(string(d), "1428.9 days");
d.Format = 'y';
assert_isequal(string(d), "3.9122 yrs");
d = seconds(0.01);
d.Format = 'h';
assert_isequal(string(d), "2.7778e-06 hr");
d.Format = 'y';
assert_isequal(string(d), "3.1689e-10 yrs");
%=============================================================================
d = seconds([1 172800 0.5]);
assert_isequal(char(d(2)), '1.728e+05 sec');
assert_isequal(cellstr(d), {'1 sec', '1.728e+05 sec', '0.5 sec'});
assert_istrue(contains(evalc('disp(d)'), '1.728e+05 sec'));
assert_isequal(string(seconds(NaN)), string(missing));
%=============================================================================
