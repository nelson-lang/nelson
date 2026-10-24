%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3 4; 5 6], 'VariableNames', {'A', 'B'});
asserts.istrue(isreal(T));
%=============================================================================
T = table([1; 2 + i], 'VariableNames', {'A'});
asserts.isfalse(isreal(T));
%=============================================================================
asserts.istrue(isreal(table()));
%=============================================================================
times = seconds([1; 2]);
TT = timetable(times, [1; 2], [3 4; 5 6], 'VariableNames', {'A', 'B'});
asserts.istrue(isreal(TT));
%=============================================================================
TTC = timetable(times, [1; 2 + i], 'VariableNames', {'A'});
asserts.isfalse(isreal(TTC));
%=============================================================================
R = TT .^ 2;
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.RowTimes, times);
asserts.isequal(R.A, [1; 4]);
asserts.isequal(R.B, [9 16; 25 36]);
%=============================================================================
E = timetable(times, [2; 3], [1 2; 3 4], 'VariableNames', {'A', 'B'});
R = TT .^ E;
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.RowTimes, times);
asserts.isequal(R.A, [1; 8]);
asserts.isequal(R.B, [3 16; 125 1296]);
%=============================================================================
R = 2 .^ TT;
asserts.isequal(R.Properties.RowTimes, times);
asserts.isequal(R.A, [2; 4]);
asserts.isequal(R.B, [8 16; 32 64]);
%=============================================================================
E2 = timetable(seconds([1; 3]), [2; 3], 'VariableNames', {'A'});
asserts.checkerror('TT .^ E2', sprintf(_('%s must match.'), 'Timetable row times'), 'Nelson:timetable:power:RowTimesMismatch');
%=============================================================================
