%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 2; 3 4];
B = [1; 0];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
z = tzero(sys);
assert_isequal(z, 4);
[z, nrank] = tzero(sys);
assert_isequal(z, 4);
assert_isapprox(nrank, 1, 1e-4);
%=============================================================================
z = tzero(A, B, C, D);
assert_isequal(z, 4)
[z, nrank] = tzero(A, B, C, D);
assert_isequal(z, 4);
assert_isapprox(nrank, 1, 1e-4);
%=============================================================================
