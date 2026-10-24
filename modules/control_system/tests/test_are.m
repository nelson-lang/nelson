%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~ismodule('slicot'), 'SLICOT is not installed');
%=============================================================================
A = [-1 0; 0 -2];
B = [1 0; 0 0];
C = eye(2);
X = are(A, B, C);
X_REF = [0.414213562373095 0; 0 0.25];
asserts.isapprox(X, X_REF, 1e-12);
%=============================================================================
A = [-3 2; 1 1];
b = [0; 1];
c = [1 -1];
X = are(A, b * b', c' * c);
X_REF = care(A, b, c' * c);
asserts.isapprox(X, X_REF, 1e-10);
%=============================================================================
asserts.checkerror('are([1 2], eye(2), eye(2))', sprintf(_('Wrong size for argument #%d: %s expected.'), 1, 'square matrix'));
asserts.checkerror('are(eye(2), [1 2; 3 4], eye(2))', sprintf(_('Wrong value for argument #%d: %s expected.'), 2, 'symmetric matrix'));
%=============================================================================
