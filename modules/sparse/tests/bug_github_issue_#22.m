%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/22
% <-- Short Description -->
% last output argument of 'IJV' did not return nzmax.
%=============================================================================
I1 = [1; 2; 3];
J1 = [3; 1; 2];
V1 = [32; 42; 53];
M1 = 5;
N1 = 4;
NZMAX1 = 10;
sp = sparse(I1, J1, V1, M1, N1, NZMAX1);
[I2, J2, V2, M2, N2, NZMAX2] = IJV(sp);
I_REF = [ 2; 3; 1];
J_REF = [ 1; 2; 3];
V_REF = [ 42; 53; 32];
M_REF = 5;
N_REF = 4;
NZMAX_REF = 10;
SP_REF = sparse(I_REF, J_REF, V_REF, M_REF, N_REF, NZMAX_REF);
assert_isequal(I_REF, I2);
assert_isequal(J_REF, J2);
assert_isequal(V_REF, V2);
assert_isequal(M_REF, M2);
assert_isequal(N_REF, N2);
assert_isequal(NZMAX_REF, NZMAX2);
assert_isequal(SP_REF, sp);
%=============================================================================
