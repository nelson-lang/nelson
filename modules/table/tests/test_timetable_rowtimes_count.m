%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% RowTimes assigned through Properties must name every row
%=============================================================================
msg = _('RowTimes property must contain one time for each row in the timetable.');
id = 'Nelson:timetable:incorrectNumberOfRowTimes';
TT = timetable(seconds([1; 2]), [3; 4]);
assert_checkerror('TT.Properties.RowTimes = seconds(1);', msg, id);
assert_checkerror('TT.Properties.RowTimes = seconds([1; 2; 3]);', msg, id);
P = TT.Properties;
P.RowTimes = seconds(1);
assert_checkerror('TT.Properties = P;', msg, id);
assert_isequal(TT.Properties.RowTimes, seconds([1; 2]));
TT.Properties.RowTimes = seconds([5 6]);
assert_isequal(TT.Properties.RowTimes, seconds([5; 6]));
%=============================================================================
% without variables, the row times still set the height
TT = timetable(seconds([1; 2]), [3; 4]);
TT(:, 1) = [];
assert_checkerror('TT.Properties.RowTimes = seconds(1);', msg, id);
assert_isequal(size(TT), [2 0]);
TT.Properties.RowTimes = seconds([7; 8]);
assert_isequal(TT.Properties.RowTimes, seconds([7; 8]));
msg = _('Value assignment must have the same number of rows as the table.');
assert_checkerror('TT.Time = seconds(1);', msg, 'Nelson:table:dimagree:valueAssignmentNumberRowsTable');
assert_isequal(size(TT), [2 0]);
TT.Time = seconds([1; 2]);
assert_isequal(TT.Properties.RowTimes, seconds([1; 2]));
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
assert_checkerror('TT.Time = seconds([1; 2; 3]);', msg, 'Nelson:table:dimagree:valueAssignmentNumberRowsTable');
%=============================================================================
