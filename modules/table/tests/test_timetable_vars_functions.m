%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% removevars, movevars, splitvars, mergevars and convertvars keep the class
% of a timetable or an eventtable.
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
R = removevars(TT, 1);
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'Var2'});
assert_isequal(R.Properties.RowTimes, seconds([1; 2]));
assert_isequal(R.Var2, [3; 4]);
%=============================================================================
R = movevars(TT, 'Var1', 'After', 'Var2');
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'Var2', 'Var1'});
assert_isequal(R.Var1, [1; 2]);
%=============================================================================
R = mergevars(TT, [1 2], 'NewVariableName', 'M');
assert_istrue(istimetable(R));
assert_isequal(R.M, [1 3; 2 4]);
R = splitvars(R, 'M');
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'M_1', 'M_2'});
assert_isequal(R.M_2, [3; 4]);
%=============================================================================
R = convertvars(TT, 1, 'single');
assert_istrue(istimetable(R));
assert_isequal(class(R.Var1), 'single');
%=============================================================================
R = renamevars(TT, 'Var1', 'z');
assert_istrue(istimetable(R));
assert_isequal(R.Properties.VariableNames, {'z', 'Var2'});
%=============================================================================
ET = eventtable(seconds([1; 2]), 'EventLabels', ["a"; "b"]);
ET.x = [5; 6];
R = removevars(ET, 'x');
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'EventLabels'});
assert_isequal(R.Properties.EventLabelsVariable, 'EventLabels');
%=============================================================================
R = removevars(ET, 'EventLabels');
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'x'});
assert_istrue(isempty(R.Properties.EventLabelsVariable));
%=============================================================================
R = movevars(ET, 'x', 'Before', 1);
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'x', 'EventLabels'});
assert_isequal(R.Properties.EventLabelsVariable, 'EventLabels');
%=============================================================================
R = convertvars(ET, 'x', 'single');
assert_isequal(class(R), 'eventtable');
assert_isequal(class(R.x), 'single');
%=============================================================================
R = renamevars(ET, 'x', 'y');
assert_isequal(class(R), 'eventtable');
assert_isequal(R.Properties.VariableNames, {'EventLabels', 'y'});
%=============================================================================
assert_checkerror('removevars(1, 1)', sprintf('Invalid input argument at position 1.\nValue must have one of these types: ''table''.'), 'Nelson:validators:mustBeA');
%=============================================================================
