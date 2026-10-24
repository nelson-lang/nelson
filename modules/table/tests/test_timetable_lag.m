%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [10; 20; 30], 'VariableNames', {'A'});
L = lag(TT);
asserts.isequal(L.Properties.RowTimes, TT.Properties.RowTimes);
asserts.istrue(isnan(L.A(1)));
asserts.isequal(L.A(2:3), [10; 20]);
L = lag(TT, -1);
asserts.isequal(L.A(1:2), [20; 30]);
asserts.istrue(isnan(L.A(3)));
%=============================================================================
