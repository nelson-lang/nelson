%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% In loose format, the body of a table, timetable or eventtable ends with an
% empty line; in compact format it ends with its last row.
%=============================================================================
currentFormat = format();
format('loose');
%=============================================================================
T = table([1; 2]);
R = evalc('disp(T)');
assert_isequal(R(end-9:end), sprintf('     2  \n\n'));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2]);
R = evalc('disp(TT)');
REF = sprintf(['    Time     Var1\n', ...
  '    _____    ____\n', ...
  '\n', ...
  '    1 sec     1  \n', ...
  '    2 sec     2  \n', ...
  '\n']);
assert_isequal(R, REF);
%=============================================================================
R = evalc('TT');
assert_isequal(R(end-18:end), sprintf('    2 sec     2  \n\n'));
%=============================================================================
E = eventtable(seconds([2; 4]));
R = evalc('disp(E)');
assert_isequal(R(end-25:end), sprintf('    4 sec     "Event 2" \n\n'));
%=============================================================================
TT.Properties.Events = E;
R = evalc('disp(TT)');
assert_isequal(R(end-29:end), sprintf('    Event 1    2 sec     2  \n\n'));
%=============================================================================
TT = timetable(seconds((1:25)'), (1:25)');
R = evalc('disp(TT)');
assert_isequal(R(end-19:end), sprintf('    25 sec     25 \n\n'));
%=============================================================================
format('compact');
R = evalc('disp(TT)');
assert_isequal(R(end-37:end), sprintf('    24 sec     24 \n    25 sec     25 \n'));
R = evalc('disp(T)');
assert_isequal(R(end-17:end), sprintf('     1  \n     2  \n'));
%=============================================================================
format(currentFormat);
%=============================================================================
