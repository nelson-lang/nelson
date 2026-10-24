%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% the display header flags a table without rows or variables as empty
%=============================================================================
T = table([1; 2]);
T(:, 1) = [];
R = evalc('T');
assert_istrue(contains(R, ['2', char(215), '0 empty table']));
%=============================================================================
T = table(zeros(0, 1), zeros(0, 1));
R = evalc('T');
assert_istrue(contains(R, ['0', char(215), '2 empty table']));
%=============================================================================
T = table(1);
R = evalc('T');
assert_istrue(contains(R, sprintf('
  table
')));
assert_isfalse(contains(R, char(215)));
assert_isfalse(contains(R, 'empty'));
%=============================================================================
TT = timetable(seconds(zeros(0, 1)), zeros(0, 1));
R = evalc('TT');
assert_istrue(contains(R, ['0', char(215), '1 empty timetable']));
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
TT(:, 1) = [];
R = evalc('TT');
assert_istrue(contains(R, ['2', char(215), '0 empty timetable']));
%=============================================================================
TT = timetable(seconds(1), 2);
R = evalc('TT');
assert_istrue(contains(R, sprintf('
  timetable
')));
assert_isfalse(contains(R, char(215)));
assert_isfalse(contains(R, 'empty'));
%=============================================================================
ET = eventtable(seconds(zeros(0, 1)));
R = evalc('ET');
assert_istrue(contains(R, 'empty eventtable'));
%=============================================================================
% disp prints no header
T = table();
R = evalc('disp(T)');
assert_isequal(R, '');
%=============================================================================
% a table without rows still shows its variable names
T = table(zeros(0, 1), zeros(0, 1), 'VariableNames', {'x', 'y'});
R = evalc('T');
assert_istrue(contains(R, 'x    y'));
R = evalc('disp(T)');
assert_istrue(contains(R, 'x    y'));
assert_istrue(contains(R, '_    _'));
TT = timetable(seconds(zeros(0, 1)), zeros(0, 1));
R = evalc('TT');
assert_istrue(contains(R, 'Time    Var1'));
R = evalc('disp(TT)');
assert_istrue(contains(R, 'Time    Var1'));
T = table([1; 2]);
T(:, 1) = [];
R = evalc('disp(T)');
assert_isequal(R, '');
%=============================================================================
