%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The array text of a datetime (string/char/cellstr/display) is rendered with
% one vectorized pass; it must stay byte-identical to the element-by-element
% rendering for every supported LDML field, for every shape and for NaT.
%=============================================================================
% A multi-element array exercises the vectorized path; check literals.
d = datetime(2021, 3, 7, 9, 5, 3) + days(0:3)';
d.Format = 'yyyy-MM-dd HH:mm:ss';
assert_isequal(string(d), ["2021-03-07 09:05:03"; "2021-03-08 09:05:03"; ...
  "2021-03-09 09:05:03"; "2021-03-10 09:05:03"]);
%=============================================================================
% Each field kind, as explicit expected text.
d = datetime(2021, 3, 7, 13, 5, 3) + milliseconds(456);
assert_isequal(string(d, 'yyyy'), "2021");
assert_isequal(string(d, 'yy'), "21");
assert_isequal(string(d, 'uuuu'), "2021");
assert_isequal(string(d, 'MMMM'), "March");
assert_isequal(string(d, 'MMM'), "Mar");
assert_isequal(string(d, 'MM'), "03");
assert_isequal(string(d, 'M'), "3");
assert_isequal(string(d, 'dd'), "07");
assert_isequal(string(d, 'd'), "7");
assert_isequal(string(d, 'EEEE'), "Sunday");
assert_isequal(string(d, 'E'), "Sun");
assert_isequal(string(d, 'HH:mm:ss'), "13:05:03");
assert_isequal(string(d, 'hh a'), "01 PM");
assert_isequal(string(d, 'h a'), "1 PM");
assert_isequal(string(d, 'SSS'), "456");
assert_isequal(string(d, 'S'), "4");
assert_isequal(string(d, 'SSSSS'), "45600");
assert_isequal(string(d, 'QQQ'), "Q1");
assert_isequal(string(d, 'QQ'), "01");
assert_isequal(string(d, 'DDD'), "066");
assert_isequal(string(d, 'yyyy ''at'' HH:mm'), "2021 at 13:05");
% An escaped single quote ('') outside a quoted run emits one literal quote.
assert_isequal(string(d, 'MM''''dd'), "03'07");
%=============================================================================
% AM boundary for the meridiem marker.
d0 = datetime(2021, 1, 1, 0, 30, 0);
assert_isequal(string(d0, 'hh:mm a'), "12:30 AM");
d12 = datetime(2021, 1, 1, 12, 0, 0);
assert_isequal(string(d12, 'hh a'), "12 PM");
%=============================================================================
% NaT elements: <missing> in string(), the text 'NaT' in cellstr/char.
d = datetime(2020, 1, 1) + days(0:4)';
d(2) = NaT;
d(5) = NaT;
d.Format = 'dd-MMM-uuuu';
s = string(d);
assert_isequal(ismissing(s), [false; true; false; false; true]);
assert_isequal(s(1), "01-Jan-2020");
c = cellstr(d);
assert_isequal(c{2}, 'NaT');
assert_isequal(c{5}, 'NaT');
assert_isequal(c{3}, '03-Jan-2020');
%=============================================================================
% Shape is preserved: scalar, row, column, matrix, N-D, empty.
assert_isequal(size(string(datetime.empty(0, 3))), [0 3]);
dm = datetime(2021, 1, 1) + days(reshape(0:11, 3, 4));
dm.Format = 'yyyy-MM-dd';
sm = string(dm);
assert_isequal(size(sm), [3 4]);
assert_isequal(sm(1, 1), "2021-01-01");
assert_isequal(sm(3, 4), "2021-01-12");
dn = datetime(2021, 1, 1) + days(reshape(0:23, 2, 3, 4));
assert_isequal(size(string(dn)), [2 3 4]);
%=============================================================================
% char of an array is a padded char matrix, one row per element.
cc = char(datetime(2020, 1, [1 2 3]));
assert_isequal(size(cc, 1), 3);
assert_isequal(strtrim(cc(1, :)), '01-Jan-2020');
%=============================================================================
