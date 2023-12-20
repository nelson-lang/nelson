%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = ss(-10, 10, 20, 0);
sysa = augstate(sys);

Ts_REF = 0;
A_REF = -10;
B_REF = 10;
C_REF = [20; 1];
D_REF = [0; 0];

assert_isequal(sysa.Ts, Ts_REF);
assert_isequal(sysa.A, A_REF);
assert_isequal(sysa.B, B_REF);
assert_isequal(sysa.C, C_REF);
assert_isequal(sysa.D, D_REF);
%=============================================================================
A = -1;
B = 1;
C = 2;
D = 0;
[Aa, Ba, Ca, Da] = augstate(A, B, C, D);

Aa_REF = -1;
Ba_REF = 1;
Ca_REF = [2; 1];
Da_REF = [0; 0];

assert_isequal(Aa, Aa_REF);
assert_isequal(Ba, Ba_REF);
assert_isequal(Ca, Ca_REF);
assert_isequal(Da, Da_REF);
%=============================================================================
sys = ss();

sysa = augstate(sys);

assert_istrue(isstatic(sysa));
%=============================================================================
