%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% TT.(name)(rows): a dynamic variable name followed by row times, a
% timerange, a withtol or text
TT = timetable(seconds([1; 2; 2.05; 3; 4]), [1; 2; 3; 4; 5], ["a"; "b"; "c"; "d"; "e"], 'VariableNames', {'A', 'S'});
name = 'A';
assert_isequal(TT.(name)(seconds(2)), 2);
assert_isequal(TT.('A')(seconds([3 2])), [4; 2]);
assert_isequal(TT.("A")(seconds(2), 1), 2);
assert_isequal(size(TT.(name)(seconds(9))), [0 1]);
assert_isequal(TT.('S')(seconds(4)), "e");
assert_isequal(TT.(name)(timerange(seconds(1), seconds(3))), [1; 2; 3]);
assert_isequal(TT.(name)(withtol(seconds(2), seconds(0.1))), [2; 3]);
assert_isequal(TT.(1)(seconds(2)), 2);
%=============================================================================
% numeric subscripts, end and following subscripts are unchanged
assert_isequal(TT.(name)(2), 2);
assert_isequal(TT.(name)(:), [1; 2; 3; 4; 5]);
assert_isequal(TT.(name)(end), 5);
assert_isequal(TT.(name)([2 3])', [2 3]);
x = TT.(name)(seconds(2)) + 1;
assert_isequal(x, 3);
%=============================================================================
TD = timetable(datetime(2020, 1, 1) + days(0:3)', (1:4)');
assert_isequal(TD.('Var1')(datetime(2020, 1, 3)), 3);
assert_isequal(TD.('Var1')('2020-01-03'), 3);
msg = sprintf(_('Timetable row subscript must be a numeric, logical, or %s array, or a timerange subscripter.'), 'datetime');
assert_checkerror('TD.(''Var1'')(seconds(1));', msg, 'Nelson:timetable:InvalidRowSubscriptsDatetime');
%=============================================================================
E = eventtable(seconds([1; 2]), 'EventLabels', ["x"; "y"]);
assert_isequal(E.('EventLabels')(seconds(2)), "y");
%=============================================================================
% tables, structs and objects keep the per-level reads
T = table([1; 2; 3], 'VariableNames', {'A'});
assert_isequal(T.(name)(2), 2);
s.A = [10 20 30];
assert_isequal(s.(name)(2), 20);
assert_isequal(s.(name)(end), 30);
c = {TT};
assert_isequal(c{1}.(name)(seconds(2)), 2);
%=============================================================================
