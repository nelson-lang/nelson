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
X = [4; 5];
[G, X] = planerot(X);
G_REF = [0.6247 0.7809; -0.7809 0.6247];
X_REF = [6.4031; 0];
asserts.isapprox(G, G_REF, 1e-4);
asserts.isapprox(X, X_REF, 1e-4);
%=============================================================================
[planeRotation, rotatedVector] = planerot([4; 5]);
asserts.isapprox(planeRotation * [4; 5], rotatedVector, 1e-12);
asserts.isapprox(rotatedVector(2), 0, 1e-12);
%=============================================================================
