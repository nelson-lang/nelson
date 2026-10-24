%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A timetable combined with an eventtable gives an eventtable, whatever the
% position of the eventtable.
%=============================================================================
TT = timetable(seconds(5), "z", seconds(6), 'VariableNames', {'EventLabels', 'EventEnds'});
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"], 'EventEnds', seconds([3; 4]));
%=============================================================================
R = [TT; ET];
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.RowTimes, seconds([5; 1; 2]));
asserts.isequal(R.EventLabels, ["z"; "a"; "b"]);
asserts.isequal(R.EventEnds, seconds([6; 3; 4]));
asserts.isequal(R.Properties.EventLabelsVariable, 'EventLabels');
asserts.isequal(R.Properties.EventEndsVariable, 'EventEnds');
asserts.isequal(R.Properties.EventLengthsVariable, []);
%=============================================================================
R = [ET; TT];
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.RowTimes, seconds([1; 2; 5]));
asserts.isequal(R.EventLabels, ["a"; "b"; "z"]);
%=============================================================================
R = [TT; TT; ET];
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.RowTimes, seconds([5; 5; 1; 2]));
asserts.isequal(class(vertcat(TT, ET)), 'eventtable');
asserts.isequal(class(cat(1, TT, ET)), 'eventtable');
asserts.isequal(class([TT; TT]), 'timetable');
%=============================================================================
TT2 = timetable(seconds([1; 2]), [3; 4], 'VariableNames', {'X'});
ET2 = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
R = [TT2, ET2];
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.VariableNames, {'X', 'EventLabels'});
asserts.isequal(R.Properties.EventLabelsVariable, 'EventLabels');
asserts.isequal(R.X, [3; 4]);
R = [ET2, TT2];
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Properties.VariableNames, {'EventLabels', 'X'});
asserts.isequal(class(horzcat(TT2, ET2)), 'eventtable');
asserts.isequal(class(cat(2, TT2, ET2)), 'eventtable');
asserts.isequal(class([TT2, timetable(seconds([1; 2]), [5; 6], 'VariableNames', {'Y'})]), 'timetable');
%=============================================================================
T3 = timetable(seconds([1; 2]), [3; 4], 'VariableNames', {'EventLabels'});
E1 = eventtable(seconds([1; 2]), 'EventLabels', [5; 6]);
R = T3 + E1;
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.EventLabels, [8; 10]);
asserts.isequal(R.Properties.EventLabelsVariable, 'EventLabels');
R = E1 + T3;
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.EventLabels, [8; 10]);
R = T3 - E1;
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.EventLabels, [-2; -2]);
asserts.isequal(class(T3 .* E1), 'eventtable');
asserts.isequal(class(E1 ./ T3), 'eventtable');
asserts.isequal(class(T3 .^ E1), 'eventtable');
asserts.isequal(class(atan2(T3, E1)), 'eventtable');
R = T3 == E1;
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.EventLabels, [false; false]);
asserts.isequal(class(E1 == E1), 'eventtable');
asserts.isequal(class(T3 < E1), 'eventtable');
asserts.isequal(class(T3 & E1), 'eventtable');
R = 2 + E1;
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.EventLabels, [7; 8]);
asserts.isequal(class(E1 - 2), 'eventtable');
asserts.isequal(class(T3 + T3), 'timetable');
%=============================================================================
