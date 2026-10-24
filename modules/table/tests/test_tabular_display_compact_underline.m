%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% In compact format, the first row of a table, timetable or eventtable body
% directly follows the underline of the headers; loose format separates them
% with an empty line.
%=============================================================================
currentFormat = format();
format('compact');
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2]);
R = evalc('TT');
REF = sprintf(['TT =\n', ...
  '  2', char(215), '1 timetable\n', ...
  '    Time     Var1\n', ...
  '    _____    ____\n', ...
  '    1 sec     1  \n', ...
  '    2 sec     2  \n']);
assert_isequal(R, REF);
%=============================================================================
R = evalc('disp(TT)');
REF = sprintf(['    Time     Var1\n', ...
  '    _____    ____\n', ...
  '    1 sec     1  \n', ...
  '    2 sec     2  \n']);
assert_isequal(R, REF);
%=============================================================================
T = table([1; 2], 'RowNames', {'a', 'b'});
R = evalc('disp(T)');
REF = sprintf(['         Var1\n', ...
  '         ____\n', ...
  '    a     1  \n', ...
  '    b     2  \n']);
assert_isequal(R, REF);
%=============================================================================
T0 = table(zeros(0, 1));
R = evalc('disp(T0)');
assert_isequal(R, sprintf('    Var1\n    ____\n'));
%=============================================================================
E = eventtable(seconds([2; 4]));
R = evalc('disp(E)');
REF = sprintf(['    Time     EventLabels\n', ...
  '    _____    ___________\n', ...
  '    2 sec     "Event 1" \n', ...
  '    4 sec     "Event 2" \n']);
assert_isequal(R, REF);
%=============================================================================
TB = timetable(seconds((1:25)'), (1:25)');
R = evalc('disp(TB)');
assert_isequal(R(1:57), sprintf(['     Time     Var1\n', ...
  '    ______    ____\n', ...
  '    1 sec       1 \n']));
%=============================================================================
format('loose');
R = evalc('disp(TT)');
REF = sprintf(['    Time     Var1\n', ...
  '    _____    ____\n', ...
  '\n', ...
  '    1 sec     1  \n', ...
  '    2 sec     2  \n', ...
  '\n']);
assert_isequal(R, REF);
%=============================================================================
format(currentFormat);
%=============================================================================
