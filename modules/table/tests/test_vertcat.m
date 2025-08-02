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
T31 = vertcat(T1,T2);
T32 = [T1; T2];
assert_isequal(T31, T32);
%=============================================================================
A = 1;
B = 2;
T1 = table(A);
T2 = table(B);
assert_checkerror('T3 = [T1; T2];', _('All tables to be vertically concatenated must have identical variable names.'));
%=============================================================================
T1 = table(1);
T2 = table([2, 3]);
assert_checkerror('T31 = vertcat(T1, T2);', _('Dimensions concatenated are not consistent.'));
%=============================================================================
T1 = table();
T2 = table(2);
R = [T1; T2];
assert_isequal(R, T2);
%=============================================================================
T2 = table();
T1 = table(2);
R = [T1; T2];
assert_isequal(R, T1);
%=============================================================================
