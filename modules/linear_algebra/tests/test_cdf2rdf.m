%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[V, D] = eig([1 2; -2 1]);
[Vr, Dr] = cdf2rdf(V, D);
assert_isapprox(abs(Vr), [0 1; 1 0], 1e-12);
assert_isapprox(Dr, [1 2; -2 1], 1e-12);
A = [1 2 0; -2 1 0; 0 0 3];
[V, D] = eig(A);
[Vr, Dr] = cdf2rdf(V, D);
assert_istrue(isreal(Vr));
assert_isapprox(A * Vr, Vr * Dr, 1e-12);
[Vr, Dr] = cdf2rdf([1 1; 2i -2i], diag([3+4i 3-4i]));
assert_isapprox(Vr, [sqrt(2) 0; 0 2 * sqrt(2)], 1e-12);
assert_isapprox(Dr, [3 4; -4 3], 1e-12);
[Vr, Dr] = cdf2rdf(eye(2), diag([2 3]));
assert_isequal(Vr, eye(2));
assert_isequal(Dr, diag([2 3]));
assert_checkerror('cdf2rdf(eye(2), diag([1+1i 2]))', _('The diagonal of D must hold real values and complex conjugate pairs.'));
%=============================================================================
