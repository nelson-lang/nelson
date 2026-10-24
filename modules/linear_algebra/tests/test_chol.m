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
asserts.isequal(nargin('chol'), 1);
asserts.isequal(nargout('chol'), 2);
%=============================================================================
A = [4 2; 2 3];
R = chol(A);
REF = [2 1; 0 sqrt(2)];
asserts.isapprox(R, REF, 1e-12);
asserts.isapprox(R' * R, A, 1e-12);
%=============================================================================
[R, p] = chol([1 2; 2 1]);
asserts.isequal(p, 2);
asserts.isequal(R, 1);
%=============================================================================
A = [4 2; 2 3];
[cholFactor, cholFlag] = chol(A);
asserts.isapprox(cholFactor' * cholFactor, A, 1e-12);
asserts.isequal(cholFlag, 0);
[badChol, badFlag] = chol([1 2; 2 1]);
asserts.isequal(badChol, 1);
asserts.isequal(badFlag, 2);
[Rnan, pnan] = chol([1 NaN; NaN 1]);
asserts.isequal(pnan, 2);
[Rinf, pinf] = chol([1 Inf; Inf 1]);
asserts.isequal(pinf, 2);
asserts.isequal(Rnan, 1);
asserts.isequal(Rinf, 1);
asserts.checkerror('chol([1 NaN; NaN 1])', sprintf(_('%s expected.'), 'Positive finite matrix'));
asserts.checkerror('chol([1 Inf; Inf 1])', sprintf(_('%s expected.'), 'Positive finite matrix'));
%=============================================================================
S = sparse(single([4 2; 2 3]));
Rs = chol(S);
REFS = single([2 1; 0 sqrt(2)]);
asserts.istrue(issparse(Rs));
asserts.isequal(class(Rs), 'single');
asserts.isapprox(full(Rs), REFS, 1e-6);
asserts.isapprox(full(Rs' * Rs), full(S), 1e-5);
%=============================================================================
H = sparse(single([4 1 + 1i; 1 - 1i 3]));
[Rcs, pcs] = chol(H);
REFCS = single([2 0.5 + 0.5i; 0 sqrt(2.5)]);
asserts.istrue(issparse(Rcs));
asserts.isequal(class(Rcs), 'single');
asserts.isequal(pcs, 0);
asserts.isapprox(full(Rcs), REFCS, 1e-6);
asserts.isapprox(full(Rcs' * Rcs), full(H), 1e-5);
%=============================================================================
[Rbad, pbad] = chol(sparse(single([1 2; 2 1])));
asserts.istrue(issparse(Rbad));
asserts.isequal(class(Rbad), 'single');
asserts.isequal(size(Rbad), [2 2]);
asserts.isequal(pbad, 2);
%=============================================================================
