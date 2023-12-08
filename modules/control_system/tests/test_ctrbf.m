%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1     1; 4    -2];
B = [1    -1; 1    -1];
C = [1     0; 0     1];

[Abar, Bbar, Cbar, T, k] = ctrbf(A, B, C);

Abar_REF = [-3.0000         0;     3.0000    2.0000];
Bbar_REF = [0         0;  -1.4142    1.4142];
Cbar_REF = [-0.7071   -0.7071; 0.7071   -0.7071];
T_REF = [-0.7071    0.7071; -0.7071   -0.7071];
k_REF = [1     0];

assert_isapprox(Abar, Abar_REF, 1e-4)
assert_isapprox(Bbar, Bbar_REF, 1e-4)
assert_isapprox(Cbar, Cbar_REF, 1e-4)
assert_isapprox(T, T_REF, 1e-4)
assert_isapprox(k, k_REF, 1e-4)
%=============================================================================
A =  [];
B =  [];
C =  [];

[Abar, Bbar, Cbar, T, k] = ctrbf(A, B, C);

assert_isequal(Abar, []);
assert_isequal(Bbar, []);
assert_isequal(Cbar, []);
assert_isequal(T, []);
assert_isequal(k, zeros(1, 0));
%=============================================================================
A = [1 2; 3 4];
B = [5; 6];
C = [7 8];
custom_tolerance = 1e-8;

[Abar, Bbar, Cbar, T, k] = ctrbf(A, B, C, custom_tolerance);

Abar_REF = [-0.2295   -1.5246;   -0.5246    5.2295];
Bbar_REF = [0.0000; -7.8102];
Cbar_REF = [-0.2561   -10.6271];
T_REF = [-0.7682    0.6402; -0.6402    -0.7682];
k_REF = [1     1];

assert_isapprox(Abar, Abar_REF, 1e-4)
assert_isapprox(Bbar, Bbar_REF, 1e-4)
assert_isapprox(Cbar, Cbar_REF, 1e-4)
assert_isapprox(T, T_REF, 1e-4)
assert_isapprox(k, k_REF, 1e-4)
%=============================================================================
A = [1     1; 4    -2];
B = [1    -1; 1    -1];
C = [1     0; 0     1];
[Abar, Bbar, Cbar, T, k] = ctrbf(A, B, C);


Abar_REF = [-3.0000         0; 3.0000    2.0000];
Bbar_REF =  [0         0;  -1.4142    1.4142];
Cbar_REF = [-0.7071   -0.7071; 0.7071   -0.7071];
T_REF = [-0.7071    0.7071; -0.7071   -0.7071];
k_REF = [1     0];
assert_isapprox(Abar, Abar_REF, 1e-4)
assert_isapprox(Bbar, Bbar_REF, 1e-4)
assert_isapprox(Cbar, Cbar_REF, 1e-4)
assert_isapprox(T, T_REF, 1e-4)
assert_isapprox(k, k_REF, 1e-4)
%=============================================================================
