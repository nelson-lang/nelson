%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2]), [10; 20], 'VariableNames', {'A'});
T = timetable2table(TT);
asserts.istrue(istable(T));
asserts.isequal(T.Time, seconds([1; 2]));
asserts.isequal(T.A, [10; 20]);
T = timetable2table(TT, 'ConvertRowTimes', false);
asserts.isequal(T.Properties.VariableNames, {'A'});
asserts.isequal(T.A, [10; 20]);
%=============================================================================
