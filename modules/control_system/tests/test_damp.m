%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = tf([2,5,1],[1,0,2,-3]);
[wn, zeta, p, T] = damp(sys);

wn_REF = [1.0000;
    1.7321;
    1.7321];

zeta_REF = [-1.0000;
    0.2887;
    0.2887];

p_REF = [1.0000 + 0.0000i;
  -0.5000 + 1.6583i;
  -0.5000 - 1.6583i];

T_REF = [-1.0000;
    2.0000;
    2.0000];

assert_isapprox(wn, wn_REF, 1e-4);
assert_isapprox(zeta, zeta_REF, 1e-4);
assert_isapprox(p, p_REF, 1e-4);
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
sys = tf([2,5,1],[1,0,2,-3]);
[wn, zeta, p, T] = damp(sys);

wn_REF = [1.0000;
    1.7321;
    1.7321];

zeta_REF = [-1.0000;
    0.2887;
    0.2887];


p_REF = [1.0000 + 0.0000i;
  -0.5000 + 1.6583i;
  -0.5000 - 1.6583i];

T_REF = [-1.0000;
    2.0000;
    2.0000];

assert_isapprox(wn, wn_REF, 1e-4);
assert_isapprox(zeta, zeta_REF, 1e-4);
assert_isapprox(p, p_REF, 1e-4);
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
