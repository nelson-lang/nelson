%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a timetable with rows but no variables displays its row times
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'A'});
TT.A = [];
assert_isequal(size(TT), [2 0]);
R = evalc('disp(TT)');
assert_istrue(contains(R, 'Time'));
assert_istrue(contains(R, '_____'));
assert_istrue(contains(R, '1 sec'));
assert_istrue(contains(R, '2 sec'));
R = evalc('TT');
assert_istrue(contains(R, ['2', char(215), '0 empty timetable']));
assert_istrue(contains(R, '_____'));
assert_istrue(contains(R, '2 sec'));
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'A'});
TT.(1) = [];
R = evalc('disp(TT)');
assert_istrue(contains(R, '1 sec'));
assert_istrue(contains(R, '2 sec'));
%=============================================================================
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
ET.EventLabels = [];
R = evalc('disp(ET)');
assert_istrue(contains(R, '1 sec'));
assert_istrue(contains(R, '2 sec'));
R = evalc('ET');
assert_istrue(contains(R, ['2', char(215), '0 empty eventtable']));
assert_istrue(contains(R, '2 sec'));
%=============================================================================
TT = timetable.empty(2, 0);
R = evalc('disp(TT)');
assert_istrue(contains(R, 'Time'));
assert_isequal(numel(strfind(R, 'NaT')), 2);
R = evalc('TT');
assert_istrue(contains(R, ['2', char(215), '0 empty timetable']));
assert_isequal(numel(strfind(R, 'NaT')), 2);
%=============================================================================
E = eventtable.empty(2, 0);
R = evalc('E');
assert_istrue(contains(R, 'Event Labels Variable: <unset>'));
assert_istrue(contains(R, 'Time'));
assert_isequal(numel(strfind(R, 'NaT')), 2);
R = evalc('disp(E)');
assert_isequal(numel(strfind(R, 'NaT')), 2);
%=============================================================================
TT = timetable(datetime(2020, 1, 1) + days(0:1)');
R = evalc('TT');
assert_istrue(contains(R, '01-Jan-2020'));
assert_istrue(contains(R, '02-Jan-2020'));
%=============================================================================
% a table with row names but no variables displays only its header
T = table('RowNames', {'a'; 'b'});
R = evalc('disp(T)');
assert_isequal(R, '');
%=============================================================================
% a timetable without rows and variables still displays nothing
TT = timetable(seconds(zeros(0, 1)));
R = evalc('disp(TT)');
assert_isequal(R, '');
%=============================================================================
