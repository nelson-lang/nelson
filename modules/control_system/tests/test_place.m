%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% SISO 2nd order system with a complex-conjugate pole pair.
A = [0 1; 0 0];
B = [0; 0.2];
p = [-4 + 5i; -4 - 5i];
K = place(A, B, p);
REF = [205 40];
assert_isapprox(K, REF, 1e-9);
assert_istrue(isreal(K));
assert_isapprox(sort(eig(A - B * K)), sort(p), 1e-9);
%=============================================================================
% SISO 3rd order system, real poles. Single input => the gain is unique.
A = [0 1 0; 0 0 1; -1 -2 -3];
B = [0; 0; 1];
p = [-1; -2; -3];
[K, prec, message] = place(A, B, p);
assert_isapprox(K, [5 9 3], 1e-9);
assert_istrue(isreal(K));
assert_istrue(isempty(message));
assert_istrue(prec > 10);
assert_isapprox(sort(eig(A - B * K)), sort(p), 1e-9);
%=============================================================================
% Two-input MIMO system: closed-loop eigenvalues must match the request and
% the gain must be real.
A = [-0.5 0 0; 0 -2 10; 0 1 -2];
B = [1 0; 0 0; 0 1];
p = [-3; -5; -7];
[K, prec] = place(A, B, p);
assert_isequal(size(K), [2 3]);
assert_istrue(isreal(K));
assert_istrue(prec > 10);
assert_isapprox(sort(eig(A - B * K)), sort(p), 1e-9);
%=============================================================================
% Multi-input system with a complex-conjugate pole pair (robust assignment).
A = [1.380 -0.2077 6.715 -5.676; ...
     -0.5814 -4.290 0 0.6750; ...
     1.067 4.273 -6.654 5.893; ...
     0.0480 4.273 1.343 -2.104];
B = [0 0; 5.679 0; 1.136 -3.146; 1.136 0];
p = [-0.2 + 0.5i; -0.2 - 0.5i; -5.0566; -8.6659];
[K, prec, message] = place(A, B, p);
assert_istrue(isreal(K));
assert_istrue(isempty(message));
assert_istrue(prec > 10);
clp = sort(eig(A - B * K));
assert_isapprox(clp, sort(p), 1e-6);
%=============================================================================
% Square, full-rank input matrix with a complex-conjugate pole pair.
A = [0 3; 2 4];
B = [-2 1; 1 1];
p = [-5 + 2i; -5 - 2i];
K = place(A, B, p);
assert_istrue(isreal(K));
assert_isapprox(sort(eig(A - B * K)), sort(p), 1e-9);
%=============================================================================
% Repeated pole allowed when its multiplicity does not exceed rank(B).
A = [1 0; 0 1];
B = [1 0; 0 1];
K = place(A, B, [-2; -2]);
assert_istrue(isreal(K));
assert_isapprox(sort(eig(A - B * K)), [-2; -2], 1e-9);
%=============================================================================
% Pole multiplicity greater than rank(B) is rejected.
A = [0 1; 0 0];
B = [0; 1];
msg = _('Pole multiplicity cannot exceed the rank of B (1).');
assert_checkerror('place([0 1; 0 0], [0; 1], [-2; -2])', msg, 'Nelson:control_system:place:multiplicity');
%=============================================================================
% Complex poles must come in conjugate pairs.
msg = _('Complex poles must be specified in conjugate pairs.');
assert_checkerror('place([0 1; 0 0], [0; 1], [-2 + 1i; -3])', msg, 'Nelson:control_system:place:complexConjugate');
%=============================================================================
% The number of poles must match the number of states.
msg = _('Poles need to have the same dimension as matrix A');
assert_checkerror('place([0 1; 0 0], [0; 1], [-1; -2; -3])', msg, 'Nelson:control_system:dimagree:polesNeedDimensionMatrixA');
%=============================================================================
