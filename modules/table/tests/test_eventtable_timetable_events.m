%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% an eventtable attached to a timetable
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
TT = timetable(seconds([1; 2; 3]), [1; 2; 3], 'Events', ET);
assert_isequal(class(TT.Properties.Events), 'eventtable');
assert_istrue(isequal(TT.Properties.Events, ET));
assert_istrue(isequal(TT(1:2, :).Properties.Events, ET));
assert_isequal(size(TT), [3 1]);
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 3]);
TT.Properties.Events = ET;
assert_isequal(TT.Properties.Events.EventLabels, ["a"; "b"]);
assert_isequal(TT.Properties.Events.Properties.RowTimes, seconds([1; 2]));
TT.Properties.Events = ET(2, :);
assert_isequal(height(TT.Properties.Events), 1);
%=============================================================================
% the events of a timetable become an eventtable
TT = timetable(seconds([1; 2]), {'a'; 'b'}, seconds([5; 6]), 'VariableNames', {'L', 'D'});
ET = eventtable(TT, 'EventLabelsVariable', 'L', 'EventLengthsVariable', 'D');
R = timetable2table(ET);
assert_isequal(class(R), 'table');
assert_isequal(R.Properties.VariableNames, {'Time', 'L', 'D'});
%=============================================================================
