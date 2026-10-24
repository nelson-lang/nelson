%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
E = eventtable(seconds([1; 2]), 'EventLabels', {'a'; 'b'}, 'EventLengths', seconds([3; 4]));
%=============================================================================
% parentheses keep an eventtable; variable positions start at the first variable
R = E(:, 1);
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'EventLabels'});
assert_isequal(R.Properties.EventLabelsVariable, 'EventLabels');
assert_isequal(R.Properties.EventLengthsVariable, []);
R = E(:, 2);
assert_isequal(R.Properties.VariableNames, {'EventLengths'});
assert_isequal(R.Properties.EventLabelsVariable, []);
assert_isequal(R.Properties.EventLengthsVariable, 'EventLengths');
R = E(2, 'EventLabels');
assert_isequal(size(R), [1 1]);
assert_isequal(R.Properties.RowTimes, seconds(2));
R = E(seconds(2), :);
assert_isequal(R.EventLabels, {'b'});
R = E(timerange(seconds(0), seconds(1.5)), :);
assert_isequal(R.EventLabels, {'a'});
R = E([], :);
assert_isequal(size(R), [0 2]);
assert_isequal(E(1, :).Properties.RowTimes, seconds(1));
%=============================================================================
% braces and dot
assert_isequal(E{:, 1}, {'a'; 'b'});
assert_isequal(E{2, 'EventLengths'}, seconds(4));
assert_isequal(E.EventLengths, seconds([3; 4]));
assert_isequal(E.Properties.RowTimes(2), seconds(2));
%=============================================================================
% assignment
E.EventLabels = {'c'; 'd'};
assert_isequal(E.EventLabels, {'c'; 'd'});
E.New = [7; 8];
assert_isequal(size(E), [2 3]);
assert_isequal(E.Properties.EventLengthsVariable, 'EventLengths');
E.Time = seconds([5; 6]);
assert_isequal(E.Properties.RowTimes, seconds([5; 6]));
E{1, 'New'} = 70;
assert_isequal(E.New, [70; 8]);
E(1, :) = [];
assert_isequal(size(E), [1 3]);
assert_isequal(class(E), 'eventtable');
%=============================================================================
% Properties assignment
E = eventtable(timetable(seconds([1; 2]), [10; 20], {'x'; 'y'}));
E.Properties.EventLabelsVariable = "Var2";
assert_isequal(E.Properties.EventLabelsVariable, 'Var2');
E.Properties.EventLabelsVariable = [];
assert_isequal(E.Properties.EventLabelsVariable, []);
E.Properties.EventLabelsVariable = 2;
assert_isequal(E.Properties.EventLabelsVariable, 'Var2');
E.Properties.Description = 'd';
assert_isequal(E.Properties.Description, 'd');
assert_isequal(E.Properties.EventLabelsVariable, 'Var2');
E.Properties.DimensionNames = {'When', 'V'};
assert_isequal(E.When, seconds([1; 2]));
P = E.Properties;
P.EventLabelsVariable = [];
P.UserData = 5;
E.Properties = P;
assert_isequal(E.Properties.EventLabelsVariable, []);
assert_isequal(E.Properties.UserData, 5);
%=============================================================================
% a renamed or removed event variable is unset
E = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"], 'EventEnds', seconds([3; 4]));
R = E;
R.Properties.VariableNames = {'L', 'EventEnds'};
assert_isequal(R.Properties.EventLabelsVariable, []);
assert_isequal(R.Properties.EventEndsVariable, 'EventEnds');
R = renamevars(E, 'EventLabels', 'L');
assert_isequal(R.Properties.EventLabelsVariable, []);
R = E;
R.EventLabels = [];
assert_isequal(R.Properties.VariableNames, {'EventEnds'});
assert_isequal(R.Properties.EventLabelsVariable, []);
R = addvars(E, [1; 2]);
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.EventLabelsVariable, 'EventLabels');
%=============================================================================
% concatenation
R = [E; E];
assert_isequal(class(R), 'eventtable');
assert_isequal(size(R), [4 2]);
assert_isequal(R.Properties.EventEndsVariable, 'EventEnds');
R = [E; timetable(seconds(5), "z", seconds(6), 'VariableNames', {'EventLabels', 'EventEnds'})];
assert_isequal(R.EventLabels, ["a"; "b"; "z"]);
R = [E(:, 1), E(:, 2)];
assert_istrue(isequal(R, E));
%=============================================================================
% math keeps the event variables
E = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
E.x = [1; 2];
R = sortrows(E, 'x', 'descend');
assert_isequal(class(R), 'eventtable');
assert_isequal(R.EventLabels, ["b"; "a"]);
E = eventtable(timetable(seconds([1; 2]), [10; 20]), 'EventLabelsVariable', 'Var1');
R = E + 1;
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Var1, [11; 21]);
assert_isequal(R.Properties.EventLabelsVariable, 'Var1');
assert_isequal(class(sum(E)), 'table');
assert_isequal(cumsum(E).Var1, [10; 30]);
assert_istrue(isequal(E, E));
assert_isfalse(isequal(E, timetable(seconds([1; 2]), [10; 20])));
%=============================================================================
