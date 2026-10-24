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
T = table.empty(0, 3);
assert_istrue(istable(T));
assert_isequal(size(T), [0 3]);
assert_isequal(T.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
assert_isequal(T.Properties.VariableTypes, ["double", "double", "double"]);
assert_istrue(isempty(T));
R = evalc('disp(T)');
assert_istrue(contains(R, 'Var1    Var2    Var3'));
%=============================================================================
T = table.empty();
assert_istrue(istable(T));
assert_isequal(size(T), [0 0]);
T = table.empty(0);
assert_isequal(size(T), [0 0]);
T = table.empty([0 3]);
assert_isequal(size(T), [0 3]);
T = table.empty([0 3 1]);
assert_isequal(size(T), [0 3]);
T = table.empty(2, 0);
assert_istrue(istable(T));
assert_isequal(size(T), [2 0]);
assert_istrue(isempty(T));
%=============================================================================
assert_checkerror('table.empty(3, 2)', _('At least one dimension must be zero to create an empty array.'), 'Nelson:emptyArray:notEmpty');
assert_checkerror('table.empty(0, 3, 2)', 'A table must be two-dimensional.', 'Nelson:table:emptyMustBeTwoDims');
%=============================================================================
TT = timetable.empty(0, 3);
assert_istrue(istimetable(TT));
assert_isequal(size(TT), [0 3]);
assert_isequal(TT.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
assert_istrue(isdatetime(TT.Properties.RowTimes));
R = evalc('disp(TT)');
assert_istrue(contains(R, 'Time    Var1    Var2    Var3'));
TT = timetable.empty();
assert_istrue(istimetable(TT));
assert_isequal(size(TT), [0 0]);
TT = timetable.empty(2, 0);
assert_isequal(size(TT), [2 0]);
assert_istrue(all(isnat(TT.Properties.RowTimes)));
assert_checkerror('timetable.empty(1, 3)', _('At least one dimension must be zero to create an empty array.'), 'Nelson:emptyArray:notEmpty');
assert_checkerror('timetable.empty(0, 3, 2)', 'A timetable must be two-dimensional.', 'Nelson:timetable:emptyMustBeTwoDims');
%=============================================================================
% a table can grow from an empty one
T = table.empty(0, 2);
T{1, :} = [1 2];
assert_isequal(T{:, :}, [1 2]);
%=============================================================================
