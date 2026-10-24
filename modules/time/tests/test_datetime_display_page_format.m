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
% disp and display of an N-D datetime resolve the default format page by
% page; string and cellstr keep one format for the whole array.
previousFormat = format();
format loose
%=============================================================================
t = cat(3, [datetime(2020, 1, 1), NaT], [datetime(2021, 1, 1, 10, 20, 30), datetime(2021, 1, 2)]);
page1 = sprintf('t(:,:,1) =\n\n   01-Jan-2020   NaT        \n');
page2 = sprintf('t(:,:,2) =\n\n   01-Jan-2021 10:20:30   02-Jan-2021 00:00:00\n');
txt = evalc('t');
assert_istrue(contains(txt, page1));
assert_istrue(contains(txt, page2));
txt = evalc('disp(t)');
assert_istrue(contains(txt, sprintf('(:,:,1) =\n\n   01-Jan-2020   NaT        \n')));
assert_istrue(contains(txt, sprintf('(:,:,2) =\n\n   01-Jan-2021 10:20:30   02-Jan-2021 00:00:00\n')));
%=============================================================================
s = string(t);
assert_isequal(s(1, 1, 1), "01-Jan-2020 00:00:00");
c = cellstr(t);
assert_isequal(c{1}, '01-Jan-2020 00:00:00');
%=============================================================================
t = cat(3, datetime(2020, 1, 1), datetime(2020, 1, 1, 1, 2, 3), datetime(2020, 1, 2));
txt = evalc('t');
assert_istrue(contains(txt, sprintf('t(:,:,1) =\n\n   01-Jan-2020\n')));
assert_istrue(contains(txt, sprintf('t(:,:,2) =\n\n   01-Jan-2020 01:02:03\n')));
assert_istrue(contains(txt, sprintf('t(:,:,3) =\n\n   02-Jan-2020\n')));
%=============================================================================
t = [datetime(2020, 1, 1), datetime(2020, 1, 1, 1, 2, 3)];
assert_istrue(contains(evalc('disp(t)'), '01-Jan-2020 00:00:00   01-Jan-2020 01:02:03'));
%=============================================================================
c = cat(3, calmonths(1), caldays(12));
txt = evalc('c');
assert_istrue(contains(txt, sprintf('  1%s1%s2 calendarDuration array\n', char(215), char(215))));
assert_istrue(contains(txt, sprintf('c(:,:,1) =\n\n   1mo\n')));
assert_istrue(contains(txt, sprintf('c(:,:,2) =\n\n   12d\n')));
%=============================================================================
format(previousFormat);
%=============================================================================
