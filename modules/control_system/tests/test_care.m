%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~ismodule('slicot'), 'SLICOT is not installed');
%=============================================================================
a = [-3 2;1 1];
b = [0 ; 1];
c = [1 -1];
r = 3;
[x, l, g] = care(a, b, c'*c, r);

x_REF = [0.5895    1.8216; 1.8216    8.8188];
l_REF = [-3.5026; -1.4370];
g_REF = [0.6072    2.9396];

assert_isapprox(x, x_REF, 1e-4);
assert_isapprox(l, l_REF, 1e-4);
assert_isapprox(g, g_REF, 1e-4);
%=============================================================================
d = 10^(-5);
A = [d+1 1; 1 d+1];
G = eye(2, 2);
Q = d^2 * eye(2,2);
B = G;
R = G;

[x, l, g] = care(A, B, Q, R); 

x_REF = [2.0000    2.0000; 2.0000    2.0000];
l_REF = [-2.0000; -0.0000];
g_REF = [2.0000    2.0000; 2.0000    2.0000];

assert_isapprox(x, x_REF, 1e-4);
assert_isapprox(l, l_REF, 1e-4);
assert_isapprox(g, g_REF, 1e-4);
%=============================================================================
A = [ 0.0  1.0; 0.0  0.0 ];
E = [ 1.0  0.0; 0.0  1.0 ];
B = [ 0.0; 1.0 ];
C = [ 1.0  0.0; 0.0  1.0; 0.0  0.0 ];
D = [ 0.0; 0.0; 1.0 ];

[x, l, g] = care (A, B, C' * C, D' * D, [], E);

x_REF = [1.7321    1.0000; 1.0000    1.7321];
l_REF = [-0.8660-0.5000i;   -0.8660 + 0.5000i];
g_REF = [1.0000    1.7321];
assert_isapprox(x, x_REF, 1e-4);
assert_isapprox(l, l_REF, 1e-4);
assert_isapprox(g, g_REF, 1e-4);
%=============================================================================
