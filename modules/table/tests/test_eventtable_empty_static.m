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
E = eventtable.empty();
assert_isequal(class(E), 'eventtable');
assert_isequal(size(E), [0 0]);
assert_istrue(isdatetime(E.Properties.RowTimes));
assert_isequal(size(E.Properties.RowTimes), [0 1]);
assert_isequal(E.Properties.EventLabelsVariable, []);
assert_isequal(E.Properties.EventLengthsVariable, []);
assert_isequal(E.Properties.EventEndsVariable, []);
R = evalc('E');
assert_istrue(contains(R, ['0', char(215), '0 empty eventtable']));
%=============================================================================
E = eventtable.empty(0);
assert_isequal(class(E), 'eventtable');
assert_isequal(size(E), [0 0]);
%=============================================================================
E = eventtable.empty(0, 3);
assert_isequal(class(E), 'eventtable');
assert_isequal(size(E), [0 3]);
assert_isequal(E.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
assert_isequal(E.Properties.VariableTypes, ["double", "double", "double"]);
assert_isequal(E.Var1, zeros(0, 0));
assert_istrue(isdatetime(E.Properties.RowTimes));
assert_istrue(isempty(E));
R = evalc('disp(E)');
assert_istrue(contains(R, 'Time    Var1    Var2    Var3'));
R = evalc('E');
assert_istrue(contains(R, ['0', char(215), '3 empty eventtable']));
%=============================================================================
E = eventtable.empty([0 3]);
assert_isequal(class(E), 'eventtable');
assert_isequal(size(E), [0 3]);
%=============================================================================
E = eventtable.empty(2, 0);
assert_isequal(class(E), 'eventtable');
assert_isequal(size(E), [2 0]);
assert_istrue(all(isnat(E.Properties.RowTimes)));
assert_istrue(isempty(E));
%=============================================================================
assert_checkerror('eventtable.empty(1, 3)', _('At least one dimension must be zero to create an empty array.'), 'Nelson:emptyArray:notEmpty');
assert_checkerror('eventtable.empty(0, 3, 2)', 'A timetable must be two-dimensional.', 'Nelson:timetable:emptyMustBeTwoDims');
%=============================================================================
