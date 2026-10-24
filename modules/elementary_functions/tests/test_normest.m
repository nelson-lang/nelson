%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.isequal(nargin('normest'), -1);
asserts.isequal(nargout('normest'), -1);
%=============================================================================
S = toeplitz([-2, 1, 0, 0]);
[R, count] = normest(S);
REF = 2.618033988437884;
asserts.isapprox(R, REF, 1e-12);
asserts.isequal(count, 4);
%=============================================================================
rng('default');
S = rand(1000);
R = normest(S, 1.0e-4);
REF = 500.4856;
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
R = sparse([1 0; 0 1; 1 1; 2 -1]);
[nrm, count] = normest(R);
asserts.isapprox(nrm, 2.5105328068866766, 1e-12);
asserts.isequal(count, 10);
%=============================================================================
C = sparse([1 -1; 1 -1]);
[nrm, count] = normest(C);
asserts.isapprox(nrm, 2, 1e-12);
asserts.isequal(count, 3);
%=============================================================================
[nrm, count] = normest(sparse(zeros(2, 3)));
asserts.isequal(nrm, 0);
asserts.isequal(count, 0);
%=============================================================================
[nrm, count] = normest(sparse(zeros(0, 0)));
asserts.isequal(nrm, 0);
asserts.isequal(count, 0);
%=============================================================================
Z = sparse([1 + 1i 0; 0 2]);
[nrm, count] = normest(Z);
asserts.isapprox(nrm, 1.9999995231632397, 1e-12);
asserts.isequal(count, 10);
%=============================================================================
RC = sparse([1 + 2i 0 -1; 0 3 4i; 2 0 0; 0 -1i 5]);
[nrm, count] = normest(RC, 1e-8);
asserts.isapprox(nrm, 7.03202981023748, 1e-12);
asserts.isequal(count, 7);
asserts.isapprox(norm(full(RC)), 7.03202981034716, 1e-12);
[nrm, count] = normest(RC, []);
asserts.isapprox(nrm, 11.5734295656041, 1e-12);
asserts.isequal(count, 0);
%=============================================================================
L = sparse(logical([true false; false true]));
[nrm, count] = normest(L);
asserts.isapprox(nrm, 1, 1e-12);
asserts.isequal(count, 2);
%=============================================================================
Ld = logical([true false; false true]);
[nrm, count] = normest(Ld);
asserts.isapprox(nrm, 1, 1e-12);
asserts.isequal(count, 2);
%=============================================================================
G = single([1 0; 0 2]);
[nrm, count] = normest(G);
asserts.isequal(class(nrm), 'single');
asserts.isapprox(nrm, single(2), 1e-6);
asserts.isequal(count, 6);
%=============================================================================
AS = sparse(single([4 1 0; 2 3 1; 0 1 2]));
[nrm, count] = normest(AS);
asserts.isequal(class(nrm), 'single');
asserts.isapprox(nrm, single(5.22941), 1e-5);
asserts.isequal(count, 6);
%=============================================================================
ZS = sparse(single([1 + 1i 0; 0 2]));
[nrm, count] = normest(ZS);
asserts.isequal(class(nrm), 'single');
asserts.isapprox(nrm, single(2), 1e-6);
asserts.isequal(count, 10);
%=============================================================================
ZCS = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
[nrm, count] = normest(ZCS);
asserts.isequal(class(nrm), 'single');
asserts.isapprox(nrm, single(2), 1e-6);
asserts.isequal(count, 1);
%=============================================================================
A = sparse([1 0; 0 2]);
[nrm, count] = normest(A, []);
asserts.isapprox(nrm, sqrt(5), 1e-12);
asserts.isequal(count, 0);
%=============================================================================
[nrm, count] = normest(A, 1 + 1i);
asserts.isapprox(nrm, sqrt(5), 1e-12);
asserts.isequal(count, 0);
%=============================================================================
[nrm, count] = normest(A, 1i);
asserts.isapprox(nrm, 2, 1e-12);
asserts.istrue(count > 0);
%=============================================================================
N = sparse([0 2 0;
            -1 0 3;
            4 0 0;
            0 5 -2]);
[nrm, count] = normest(N, 1e-8);
asserts.isapprox(nrm, 5.8230441964765163, 1e-12);
asserts.isequal(count, 14);
%=============================================================================
asserts.checkerror('normest(ones(2, 2, 2))', _('An numeric 2-D matrix expected.'));
asserts.checkerror('normest(eye(2), {1})', sprintf(_('%s must be a numeric scalar.'), 'tolerance'));
asserts.checkerror('normest(eye(2), -1)', _('tolerance must be a nonnegative finite scalar.'));
asserts.checkerror('normest(eye(2), Inf)', _('tolerance must be a nonnegative finite scalar.'));
asserts.checkerror('normest(eye(2), NaN)', _('tolerance must be a nonnegative finite scalar.'));
asserts.checkerror('normest(uint8(eye(2)))', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
%=============================================================================
