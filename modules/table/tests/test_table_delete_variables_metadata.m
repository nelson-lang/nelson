%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Deleting variables removes their units, descriptions and continuity.
%=============================================================================
T = table([1; 2], [3; 4], [5; 6]);
T.Properties.VariableUnits = {'m', 's', 'kg'};
T.Properties.VariableDescriptions = {'a', 'b', 'c'};
%=============================================================================
R = removevars(T, 1);
assert_isequal(R.Properties.VariableUnits, {'s', 'kg'});
assert_isequal(R.Properties.VariableDescriptions, {'b', 'c'});
%=============================================================================
R = T;
R(:, [1 3]) = [];
assert_isequal(R.Properties.VariableNames, {'Var2'});
assert_isequal(R.Properties.VariableUnits, {'s'});
assert_isequal(R.Properties.VariableDescriptions, {'b'});
%=============================================================================
R = T;
R.Var2 = [];
assert_isequal(R.Properties.VariableUnits, {'m', 'kg'});
assert_isequal(R.Properties.VariableDescriptions, {'a', 'c'});
%=============================================================================
R = T;
R(:, 1:3) = [];
assert_isequal(size(R.Properties.VariableUnits), [1 0]);
assert_isequal(size(R.Properties.VariableDescriptions), [1 0]);
%=============================================================================
R = T;
R(1, :) = [];
assert_isequal(R.Properties.VariableUnits, {'m', 's', 'kg'});
%=============================================================================
U = table([1; 2], [3; 4]);
U.Properties.VariableUnits = {'m', ''};
R = removevars(U, 1);
assert_isequal(R.Properties.VariableUnits, {''});
%=============================================================================
R = removevars(table([1; 2], [3; 4]), 1);
assert_isequal(R.Properties.VariableUnits, {});
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableContinuity = ["step", "continuous"];
TT.Properties.VariableUnits = {'m', 's'};
%=============================================================================
R = TT;
R(:, 1) = [];
assert_isequal(string(R.Properties.VariableContinuity), "continuous");
assert_isequal(R.Properties.VariableUnits, {'s'});
%=============================================================================
R = removevars(TT, 2);
assert_isequal(string(R.Properties.VariableContinuity), "step");
assert_isequal(R.Properties.VariableUnits, {'m'});
%=============================================================================
R = TT;
R.Var1 = [];
assert_isequal(string(R.Properties.VariableContinuity), "continuous");
%=============================================================================
R = TT;
R(1, :) = [];
assert_isequal(string(R.Properties.VariableContinuity), ["step", "continuous"]);
%=============================================================================
E = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
E.x = [1; 2];
E.y = [3; 4];
E.Properties.VariableUnits = {'', 'm', 's'};
R = removevars(E, 'x');
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableUnits, {'', 's'});
R = E;
R(:, 'y') = [];
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableUnits, {'', 'm'});
assert_isequal(R.Properties.EventLabelsVariable, 'EventLabels');
%=============================================================================
