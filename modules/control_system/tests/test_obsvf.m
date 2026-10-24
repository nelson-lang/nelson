%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 1; 4 -2];
B = [1 -1; 1 -1];
C = [1 0; 0 1];
[Abar, Bbar, Cbar, T, k] = obsvf(A, B, C);

Abar_REF = [1 1; 4 -2];
Bbar_REF = [1 -1; 1 -1];
Cbar_REF = [1 0; 0 1];
T_REF = [1 0; 0 1];
k_REF = [2 0];

asserts.isapprox(Abar, Abar_REF, 1e-4)
asserts.isapprox(Bbar, Bbar_REF, 1e-4)
asserts.isapprox(Cbar, Cbar_REF, 1e-4)
asserts.isapprox(T, T_REF, 1e-4)
asserts.isapprox(k, k_REF, 1e-4)
%=============================================================================
