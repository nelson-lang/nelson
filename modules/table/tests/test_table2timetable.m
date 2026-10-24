%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table(seconds([1; 2]), [5; 6], 'VariableNames', {'Time', 'Value'});
TT = table2timetable(T);
asserts.istrue(istimetable(TT));
asserts.isequal(TT.Properties.RowTimes, seconds([1; 2]));
asserts.isequal(TT.Value, [5; 6]);
TT = table2timetable(table([7; 8]), 'RowTimes', seconds([3; 4]));
asserts.isequal(TT.Properties.RowTimes, seconds([3; 4]));
asserts.isequal(TT.Var1, [7; 8]);
%=============================================================================
