%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sparse divisors with a dense band use a band LU, singular or not.
M = [1 2 3; 4 5 6; 7 8 9];
%=============================================================================
% M is singular: which exact solution comes out depends on FMA in the BLAS kernels.
lastwarn('', '');
X = sparse(M) \ [1; 2; 3];
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(all(isfinite(X)));
asserts.istrue(norm(M * X - [1; 2; 3]) < 1e-12);
%=============================================================================
lastwarn('', '');
X = sparse(M) \ sparse([1; 2; 3]);
[msg, id] = lastwarn();
asserts.istrue(issparse(X));
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(all(isfinite(full(X))));
asserts.istrue(norm(M * full(X) - [1; 2; 3]) < 1e-12);
%=============================================================================
% '/' factors the divisor itself and solves with its transpose.
lastwarn('', '');
Y = [1 2 3] / sparse(M);
[msg, id] = lastwarn();
asserts.isfalse(issparse(Y));
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(norm(Y - [1 0 0]) < 1e-12);
%=============================================================================
Y = sparse([1 2 3]) / sparse(M);
asserts.istrue(issparse(Y));
asserts.istrue(norm(full(Y) - [1 0 0]) < 1e-12);
%=============================================================================
lastwarn('', '');
X = sparse(M') \ [1; 2; 3];
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isequal(msg, _('Matrix is singular to working precision.'));
asserts.isfalse(any(isfinite(X)));
%=============================================================================
lastwarn('', '');
Y = [1 2 3; 4 5 6] / sparse(M');
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isfalse(any(isfinite(Y(:))));
%=============================================================================
lastwarn('', '');
X = sparse(single(M)) \ single([1; 2; 3]);
[msg, id] = lastwarn();
asserts.isequal(class(X), 'single');
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(norm(single(M) * X - single([1; 2; 3])) < 1e-4);
%=============================================================================
lastwarn('', '');
Y = single([1 2 3]) / sparse(single(M));
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(norm(Y * single(M) - single([1 2 3])) < 1e-4);
%=============================================================================
lastwarn('', '');
X = sparse(single(M')) \ single([1; 2; 3]);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.isequal(class(X), 'single');
asserts.isfalse(any(isfinite(X)));
%=============================================================================
% Regular band systems give no warning.
A = speye(5) * 4;
A(1, :) = 1;
A(:, 1) = 1;
A(1, 1) = 5;
REF = [-0.625 0.65625 0.90625 1.15625 1.40625];
lastwarn('', '');
X = A \ (1:5)';
Y = (1:5) / A;
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isapprox(X, REF', 1e-14);
asserts.isapprox(Y, REF, 1e-14);
%=============================================================================
C = sparse([1 2i 3; 4 5 6; 7 8 9]);
lastwarn('', '');
Y = [1 2 3] / C;
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.istrue(norm(Y - [0 2 -1]) < 1e-14);
%=============================================================================
Z = sparse([1 1i 1; 1i 1 1; 1 1 1i]);
asserts.isapprox([1 2 3] / Z, [0.8 - 0.4i, 1.3 + 0.1i, 0.3 - 0.9i], 1e-14);
asserts.isapprox(Z \ [1; 2; 3], [0.8 - 0.4i; 1.3 + 0.1i; 0.3 - 0.9i], 1e-14);
%=============================================================================
