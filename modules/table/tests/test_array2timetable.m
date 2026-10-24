%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = array2timetable([1 2; 3 4], 'RowTimes', seconds([10; 20]), ...
  'VariableNames', {'A', 'B'});
asserts.istrue(istimetable(TT));
asserts.isequal(TT.Properties.RowTimes, seconds([10; 20]));
asserts.isequal(TT.A, [1; 3]);
asserts.isequal(TT.B, [2; 4]);
%=============================================================================
TT = array2timetable(int8([1 3; 2 4]), 'RowTimes', seconds([1; 2]));
asserts.isequal(TT.Properties.VariableTypes, ["int8", "int8"]);
asserts.istrue(isequal(+TT, TT));
asserts.istrue(isequal(-(-TT), TT));
asserts.istrue(isequal(TT(1:2, :), TT));
%=============================================================================
