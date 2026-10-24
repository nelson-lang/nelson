%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The shared time-array display pads every element to the widest one and
% separates columns with three blanks. duration/calendarDuration right-align;
% datetime left-aligns. This locks the vectorized matrixText output.
%=============================================================================
% Right-aligned (duration): elements padded on the LEFT to the widest.
du = seconds([1 22 333]');
txt = evalc('disp(du)');
expected = ['     1 sec', newline, '    22 sec', newline, '   333 sec', newline];
assert_istrue(contains(txt, expected));
%=============================================================================
% Left-aligned (datetime): the short NaT element is padded on the RIGHT.
d = datetime(2020, 1, [1 2 3]);
d(2) = NaT;
d.Format = 'dd-MMM-uuuu';
txt = evalc('disp(d)');
expected = ['   01-Jan-2020   NaT           03-Jan-2020', newline];
assert_istrue(contains(txt, expected));
%=============================================================================
% A matrix keeps one line per row, three blanks between columns.
d = datetime(2020, 1, 1) + hours([0 1 2; 3 4 5]);
d.Format = 'yyyy-MM-dd HH:mm:ss';
txt = evalc('disp(d)');
expected = ['   2020-01-01 00:00:00   2020-01-01 01:00:00   2020-01-01 02:00:00', newline, ...
  '   2020-01-01 03:00:00   2020-01-01 04:00:00   2020-01-01 05:00:00', newline];
assert_istrue(contains(txt, expected));
%=============================================================================
% Scalar still shows the single element with no trailing column padding.
txt = evalc('disp(datetime(2021, 6, 15))');
assert_istrue(contains(txt, '15-Jun-2021'));
%=============================================================================
