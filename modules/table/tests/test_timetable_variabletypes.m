%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], int8([3; 4]));
assert_isequal(TT.Properties.VariableTypes, ["double", "int8"]);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
assert_isequal(TT.Properties.VariableTypes, ["double", "double"]);
assert_istrue(isequal(+TT, TT));
assert_istrue(isequal(-(-TT), TT));
assert_istrue(isequal(abs(TT), TT));
assert_istrue(isequal(TT(1:2, :), TT));
%=============================================================================
TT2 = timetable(seconds([1; 2]), [1; 2]);
TT2.Var2 = [3; 4];
assert_istrue(isequal(TT2, TT));
%=============================================================================
T = table([1; 2], [3; 4]);
assert_isequal(T.Properties.VariableTypes, ["double", "double"]);
assert_istrue(isequal(+T, T));
assert_istrue(isequal(-(-T), T));
assert_istrue(isequal(table2timetable(T, 'RowTimes', seconds([1; 2])), TT));
%=============================================================================
TT3 = timetable('Size', [2 2], 'VariableTypes', {'double', 'logical'}, 'RowTimes', seconds([1; 2]));
assert_isequal(TT3.Properties.VariableTypes, ["double", "logical"]);
%=============================================================================
TT4 = timetable();
assert_isequal(TT4.Properties.VariableTypes, string([]));
%=============================================================================
