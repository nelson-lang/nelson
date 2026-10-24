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
% duration, datetime and calendarDuration arrays display as matrices, N-D
% arrays page by page, with a class header naming the size.
x = char(215);
previousFormat = format();
format loose
%=============================================================================
d = cat(3, seconds(1), 2);
assert_isequal(size(d), [1 1 2]);
assert_istrue(contains(evalc('d'), sprintf('d(:,:,2) =\n')));
d = cat(3, seconds(1), seconds(2));
R = evalc('d');
REF = sprintf(['\n  1', x, '1', x, '2 duration array\n\nd(:,:,1) =\n\n   1 sec\n\n', ...
  '\nd(:,:,2) =\n\n   2 sec\n\n']);
assert_isequal(R, REF);
%=============================================================================
R = evalc('disp(d)');
REF = sprintf('\n(:,:,1) =\n\n   1 sec\n\n\n(:,:,2) =\n\n   2 sec\n\n');
assert_isequal(R, REF);
%=============================================================================
z = seconds(ones(2, 2, 2));
R = evalc('z');
PAGE = sprintf('   1 sec   1 sec\n   1 sec   1 sec\n');
REF = sprintf(['\n  2', x, '2', x, '2 duration array\n\nz(:,:,1) =\n\n', PAGE, ...
  '\n\nz(:,:,2) =\n\n', PAGE, '\n']);
assert_isequal(R, REF);
%=============================================================================
% page widths are computed page by page
a = seconds(cat(3, [1 2], [300 4]));
R = evalc('disp(a)');
REF = sprintf(['\n(:,:,1) =\n\n   1 sec   2 sec\n\n', ...
  '\n(:,:,2) =\n\n   300 sec     4 sec\n\n']);
assert_isequal(R, REF);
%=============================================================================
h = hours(cat(4, 1, 2));
R = evalc('h');
REF = sprintf(['\n  1', x, '1', x, '1', x, '2 duration array\n\n', ...
  'h(:,:,1,1) =\n\n   1 hr\n\n\nh(:,:,1,2) =\n\n   2 hr\n\n']);
assert_isequal(R, REF);
%=============================================================================
% empty N-D arrays show only the header; disp shows nothing
e1 = duration.empty(1, 0, 2);
R = evalc('e1');
assert_isequal(R, sprintf(['\ne1 = \n\n  1', x, '0', x, '2 empty duration array\n\n']));
assert_isequal(evalc('disp(e1)'), '');
e2 = duration.empty(0, 0, 3);
R = evalc('e2');
assert_isequal(R, sprintf(['\ne2 = \n\n  0', x, '0', x, '3 empty duration array\n\n']));
assert_isequal(evalc('disp(e2)'), '');
%=============================================================================
% matrices: one common width for every element, right aligned
m = seconds([1 NaN -300]);
R = evalc('m');
REF = sprintf(['\nm = \n\n  1', x, '3 duration array\n\n', ...
  '      1 sec    NaN sec   -300 sec\n\n']);
assert_isequal(R, REF);
assert_isequal(evalc('disp(m)'), sprintf('      1 sec    NaN sec   -300 sec\n\n'));
%=============================================================================
s = seconds(5);
assert_isequal(evalc('s'), sprintf('\ns = \n\n  duration\n\n   5 sec\n\n'));
assert_isequal(evalc('disp(s)'), sprintf('   5 sec\n\n'));
%=============================================================================
% datetime: left aligned, same page layout
b = [datetime(2020, 1, 1) NaT; NaT datetime(2020, 1, 2, 3, 4, 5)];
R = evalc('disp(b)');
REF = sprintf(['   01-Jan-2020 00:00:00   NaT                 \n', ...
  '   NaT                    02-Jan-2020 03:04:05\n\n']);
assert_isequal(R, REF);
c = reshape(datetime(2020, 1, 1:2), 1, 1, 2);
R = evalc('c');
REF = sprintf(['\n  1', x, '1', x, '2 datetime array\n\nc(:,:,1) =\n\n   01-Jan-2020\n\n', ...
  '\nc(:,:,2) =\n\n   02-Jan-2020\n\n']);
assert_isequal(R, REF);
%=============================================================================
% calendarDuration: right aligned, same page layout
k2 = [calmonths(1) caldays(12); calyears(3) calmonths(14)];
R = evalc('disp(k2)');
assert_isequal(R, sprintf('      1mo      12d\n       3y   1y 2mo\n\n'));
k = reshape([calmonths(1) caldays(12)], 1, 1, 2);
R = evalc('k');
REF = sprintf(['\n  1', x, '1', x, '2 calendarDuration array\n\nk(:,:,1) =\n\n   1mo\n\n', ...
  '\nk(:,:,2) =\n\n   12d\n\n']);
assert_isequal(R, REF);
%=============================================================================
format compact
R = evalc('d');
REF = sprintf(['  1', x, '1', x, '2 duration array\nd(:,:,1) =\n   1 sec\n', ...
  'd(:,:,2) =\n   2 sec\n']);
assert_isequal(R, REF);
assert_isequal(evalc('disp(d)'), sprintf('(:,:,1) =\n   1 sec\n(:,:,2) =\n   2 sec\n'));
assert_isequal(evalc('e1'), sprintf(['e1 = \n  1', x, '0', x, '2 empty duration array\n']));
assert_isequal(evalc('disp(m)'), sprintf('      1 sec    NaN sec   -300 sec\n'));
%=============================================================================
format(previousFormat);
%=============================================================================
