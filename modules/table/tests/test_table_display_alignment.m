%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% table, timetable and eventtable display: no trailing separator, headers
% centered, numbers and times right-aligned and texts left-aligned in a block
% centered under the header, row labels left-aligned.
%=============================================================================
format('compact');
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'A'});
TT.A = [];
R = evalc('disp(TT)');
REF = sprintf('    Time \n    _____\n    1 sec\n    2 sec\n');
assert_isequal(R, REF);
%=============================================================================
E = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
R = evalc('disp(E)');
REF = sprintf(['    Time     EventLabels\n', ...
  '    _____    ___________\n', ...
  '    1 sec        "a"    \n', ...
  '    2 sec        "b"    \n']);
assert_isequal(R, REF);
%=============================================================================
T = table([1; 22; 333], ["a"; "bbb"; "a"], [true; false; true], ...
  'VariableNames', {'LongNumberName', 'LongStringName', 'LongLogical'});
REF = sprintf(['    LongNumberName    LongStringName    LongLogical\n', ...
  '    ______________    ______________    ___________\n', ...
  '           1              "a"              true    \n', ...
  '          22              "bbb"            false   \n', ...
  '         333              "a"              true    \n']);
R = evalc('disp(T)');
assert_isequal(R, REF);
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1'; 'rowtwo'});
R = evalc('disp(T)');
REF = sprintf(['              A    B\n', ...
  '              _    _\n', ...
  '    r1        1    3\n', ...
  '    rowtwo    2    4\n']);
assert_isequal(R, REF);
%=============================================================================
T = table(["abcdefgh"; "b"], 'VariableNames', {'A'});
R = evalc('disp(T)');
REF = sprintf(['        A     \n', ...
  '    __________\n', ...
  '    "abcdefgh"\n', ...
  '    "b"       \n']);
assert_isequal(R, REF);
%=============================================================================
T = table(seconds([1; 200]), categorical({'a'; 'bbb'}), ...
  'VariableNames', {'LongDurationName', 'LongCategorical'});
R = evalc('disp(T)');
REF = sprintf(['    LongDurationName    LongCategorical\n', ...
  '    ________________    _______________\n', ...
  '          1 sec               a        \n', ...
  '        200 sec               bbb      \n']);
assert_isequal(R, REF);
%=============================================================================
TT = timetable(seconds([1; 200]), [1; 2], 'VariableNames', {'A'});
R = evalc('disp(TT)');
REF = sprintf(['     Time      A\n', ...
  '    _______    _\n', ...
  '    1 sec      1\n', ...
  '    200 sec    2\n']);
assert_isequal(R, REF);
%=============================================================================
