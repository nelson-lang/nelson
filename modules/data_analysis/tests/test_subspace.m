%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
H = hadamard(8);
A = H(:,2:4);
B = H(:,5:8);
theta = subspace(A,B);
expectedTheta = pi/2;
tol = 1e-10;
assert_istrue(abs(theta - expectedTheta) < tol);
%=============================================================================
