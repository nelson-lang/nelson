%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.isequal(nargin('realpow'), 2);
asserts.isequal(nargout('realpow'), 1);
%=============================================================================
R = realpow(2, 3);
asserts.isequal(R, 8);
%=============================================================================
X = -2 * ones(3, 3);
Y = pascal(3);
R = realpow(X, Y);
REF = [-2 -2 -2; -2 4 -8; -2 -8 64];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
A = 1:3;
B = (2:4)';
R = realpow(A, B);
REF = [1 4 9; 1 8 27; 1 16 81];
asserts.isequal(R, REF);
%=============================================================================
R = realpow(int8([2 3]), int8([3 2]));
asserts.isequal(R, int8([8 9]));
%=============================================================================
R = realpow([true false], 2);
asserts.isequal(R, [1 0]);
%=============================================================================
asserts.checkerror('realpow(1 + i, 2)', sprintf(_('%s must be real.'), 'Inputs'), 'Nelson:validators:mustBeReal');
asserts.checkerror('realpow(2, 1 + i)', sprintf(_('%s must be real.'), 'Inputs'), 'Nelson:validators:mustBeReal');
asserts.checkerror('realpow(-1, 0.5)', sprintf(_('%s must be real.'), 'Result'), 'Nelson:validators:mustBeReal');
%=============================================================================
T = table([-2; -3], [2; 4], 'VariableNames', {'A', 'B'});
R = realpow(T, 2);
asserts.istrue(istable(R));
asserts.isequal(R.A, [4; 9]);
asserts.isequal(R.B, [4; 16]);
%=============================================================================
TE = table([1; 2], [2; 3], 'VariableNames', {'A', 'B'});
R = realpow(T, TE);
asserts.isequal(R.A, [-2; 9]);
asserts.isequal(R.B, [4; 64]);
%=============================================================================
TC = table([1; 1 + i], 'VariableNames', {'A'});
asserts.checkerror('realpow(TC, 2)', sprintf(_('%s must be real.'), 'Inputs'), 'Nelson:validators:mustBeReal');
asserts.checkerror('realpow(table([-1; -4], ''VariableNames'', {''A''}), 0.5)', sprintf(_('%s must be real.'), 'Result'), 'Nelson:validators:mustBeReal');
%=============================================================================
times = seconds([1; 2]);
TT = timetable(times, [-2; -3], [2; 4], 'VariableNames', {'A', 'B'});
R = realpow(TT, 2);
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.RowTimes, times);
asserts.isequal(R.A, [4; 9]);
asserts.isequal(R.B, [4; 16]);
%=============================================================================
TTE = timetable(times, [1; 2], [2; 3], 'VariableNames', {'A', 'B'});
R = realpow(TT, TTE);
asserts.isequal(R.Properties.RowTimes, times);
asserts.isequal(R.A, [-2; 9]);
asserts.isequal(R.B, [4; 64]);
%=============================================================================
TTC = timetable(times, [1; 1 + i], 'VariableNames', {'A'});
asserts.checkerror('realpow(TTC, 2)', sprintf(_('%s must be real.'), 'Inputs'), 'Nelson:validators:mustBeReal');
asserts.checkerror('realpow(timetable(times, [-1; -4], ''VariableNames'', {''A''}), 0.5)', sprintf(_('%s must be real.'), 'Result'), 'Nelson:validators:mustBeReal');
%=============================================================================
