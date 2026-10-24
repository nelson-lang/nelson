%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a cell row assigned into a timetable fills duration variables
TT = timetable(seconds([1; 2]), ["a"; "b"], seconds([3; 4]));
TT(1, :) = {"r", seconds(1)};
assert_isequal(TT.Var1, ["r"; "b"]);
assert_isequal(class(TT.Var2), 'duration');
assert_isequal(seconds(TT.Var2), [1; 4]);
assert_isequal(seconds(TT.Properties.RowTimes), [1; 2]);
%=============================================================================
% same with a table, for each time and categorical variable type
T = table(["a"; "b"], seconds([3; 4]), datetime(2020, 1, [1; 2]), categorical(["x"; "y"]), [1; 2]);
T(2, :) = {"r", minutes(1), datetime(2021, 5, 6), categorical("y"), 7};
assert_isequal(T.Var1, ["a"; "r"]);
assert_isequal(class(T.Var2), 'duration');
assert_isequal(seconds(T.Var2), [3; 60]);
assert_isequal(class(T.Var3), 'datetime');
assert_isequal(T.Var3(2), datetime(2021, 5, 6));
assert_isequal(T.Var3(1), datetime(2020, 1, 1));
assert_isequal(class(T.Var4), 'categorical');
assert_isequal(string(T.Var4), ["x"; "y"]);
assert_isequal(T.Var5, [1; 7]);
%=============================================================================
% cell2table concatenates columns of such elements
C = cell2table({seconds(1); seconds(2)});
assert_isequal(class(C.Var1), 'duration');
assert_isequal(seconds(C.Var1), [1; 2]);
C = cell2table({datetime(2020, 1, 1); datetime(2020, 1, 2)});
assert_isequal(class(C.Var1), 'datetime');
C = cell2table({categorical("a"); categorical("b")});
assert_isequal(class(C.Var1), 'categorical');
%=============================================================================
% elements that do not concatenate stay a cell
C = cell2table({seconds(1); datetime(2020, 1, 1)});
assert_isequal(class(C.Var1), 'cell');
%=============================================================================
