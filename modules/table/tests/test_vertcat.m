%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T1 = table(1);
T2 = table(2);
T31 = vertcat(T1, T2);
T32 = [T1; T2];
asserts.isequal(T31, T32);
%=============================================================================
A = 1;
B = 2;
T1 = table(A);
T2 = table(B);
asserts.checkerror('T3 = [T1; T2];', _('All tables to be vertically concatenated must have identical variable names.'));
%=============================================================================
T1 = table(1);
T2 = table([2, 3]);
asserts.checkerror('T31 = vertcat(T1, T2);', _('Dimensions of arrays being concatenated are not consistent.'));
%=============================================================================
T1 = table();
T2 = table(2);
R = [T1; T2];
asserts.isequal(R, T2);
%=============================================================================
T2 = table();
T1 = table(2);
R = [T1; T2];
asserts.isequal(R, T1);
%=============================================================================
% RowNames stays a column (N x 1) after vertical concatenation.
TA = table([1; 2], 'RowNames', {'a', 'b'});
TB = table([3; 4], 'RowNames', {'c', 'd'});
R = [TA; TB];
asserts.isequal(size(R.Properties.RowNames), [4 1]);
asserts.isequal(R.Properties.RowNames, {'a'; 'b'; 'c'; 'd'});
%=============================================================================
