%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 2; 3 4];
[Q, T] = schur(A);
[Qo, To] = schord(Q, T, [2 1]);
To_REF = [5.37228132326901 1; -1.11022302462516e-16 -0.372281323269014];
Qo_REF = [-0.415973557919284 -0.909376709132124; -0.909376709132124 0.415973557919284];
asserts.isapprox(To, To_REF, 1e-12);
asserts.isapprox(abs(Qo), abs(Qo_REF), 1e-12);
asserts.isapprox(Qo * To * Qo', A, 1e-12);
%=============================================================================
Ti = diag([3 1 2]);
Qi = eye(3);
[Qo, To] = schord(Qi, Ti, [3 1 2]);
asserts.isapprox(diag(To).', [1 2 3], 1e-12);
asserts.isapprox(Qo * To * Qo', Ti, 1e-12);
%=============================================================================
asserts.checkerror('schord(eye(2), [1 2], [1 2])', sprintf(_('Wrong size for argument #%d: %s expected.'), 2, 'square matrix'));
asserts.checkerror('schord(eye(2), eye(2), [1 2 3])', _('Wrong size for argument #3: vector length must match matrix size.'));
%=============================================================================
