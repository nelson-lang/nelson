%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('condeig'), -1);
asserts.isequal(nargout('condeig'), -1);
%=============================================================================
A = [10, 20; 30, 40];
[X, D, S] = condeig(A);
X_REF = [-0.8246 -0.4160;
  0.5658 -0.9094];
D_REF = [-3.7228 0; 0 53.7228];
S_REF = [1.0150; 1.0150];
asserts.isapprox(X, X_REF, 1e-4);
asserts.isapprox(D, D_REF, 1e-4);
asserts.isapprox(S, S_REF, 1e-4);
%=============================================================================
A = [10, 20; 30, 40];
S = condeig(A);
S_REF = [1.0150; 1.0150];
asserts.isapprox(S, S_REF, 1e-4);
%=============================================================================
[condeigVectors, condeigValues, condeigCondition] = condeig([10 20; 30 40]);
asserts.isapprox(condeigVectors, [-0.8246 -0.4160; 0.5658 -0.9094], 1e-4);
asserts.isapprox(condeigValues, [-3.7228 0; 0 53.7228], 1e-4);
asserts.isapprox(condeigCondition, [1.0150; 1.0150], 1e-4);
%=============================================================================
