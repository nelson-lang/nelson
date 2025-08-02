%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s1 = ss(1);
A = [-15, -20; 10, 0];
B = [5; 0];
C = [0, 10];
D = 0;
s2 = ss(A, B, C, D);
s3 = ss(A, B, C, D, 0.2);
assert_istrue(isequal(s1, s1, s1));
assert_isfalse(isequal(s1, s2, s1));
assert_istrue(isequal(s1, s1));
assert_isfalse(isequal(s2, s3));
%=============================================================================
