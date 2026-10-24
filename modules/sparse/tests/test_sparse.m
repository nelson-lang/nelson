%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([]);
asserts.isequal(size(A), [0 0]);
asserts.isequal(class(A), 'double');
asserts.istrue(issparse(A));
A(10, 11) = 1;
asserts.isequal(size(A), [10 11]);
[a, b, c, d, e] = IJV(A);
ref_a = 10;
ref_b = 11;
ref_c = 1;
ref_d = 10;
ref_e = 11;
asserts.isequal(a, ref_a);
asserts.isequal(b, ref_b);
asserts.isequal(c, ref_c);
asserts.isequal(d, ref_d);
asserts.isequal(e, ref_e);
%=============================================================================
a = logical(sparse(1));
asserts.isequal(class(a), 'logical');
asserts.isequal(size(a), [1 1]);
asserts.istrue(full(a(1, 1)));
asserts.istrue(issparse(a));
%=============================================================================
asserts.checkerror('S = sparse(zeros(3, 3), 3, 3)', _('in I, J, V format, all three vectors must be the same size or be scalars.'));
% the I, J, V size condition also carries a stable identifier
asserts.checkerror('S = sparse(zeros(3, 3), 3, 3)', _('in I, J, V format, all three vectors must be the same size or be scalars.'), 'Nelson:sparse:ijvSizeMismatch');
%=============================================================================
R = sparse([], 4, 4);
REF = sparse([]);
asserts.isequal(R, REF);
%=============================================================================
R = sparse([], 4, 4, 5, 6);
S = size(R);
asserts.isequal(S, [5, 6]);
%=============================================================================
S = sparse([1 0; 0 2]);
asserts.isequal(size(S), [2 2]);
asserts.isequal(full(S), [1 0; 0 2]);
emptySparse = sparse(zeros(0, 3));
asserts.isequal(size(emptySparse), [0 3]);
%=============================================================================
S = sparse(1, 1);
asserts.isequal(full(S), 0);
S = sparse(1, 1, 1);
asserts.isequal(full(S), 1);
S = sparse(1, 1, single(1 + 2i));
asserts.isequal(class(S), 'single');
asserts.isapprox(full(S), single(1 + 2i), 1e-6);
%=============================================================================
S = sparse(single([1 0; 0 2]));
asserts.isequal(class(S), 'single');
asserts.istrue(issparse(S));
asserts.isequal(size(S), [2 2]);
asserts.isequal(full(S), single([1 0; 0 2]));
%=============================================================================
S = sparse(1:3, 1:3, sparse(2), 3, 3);
asserts.istrue(issparse(S));
asserts.isequal(full(S), 2 * eye(3, 3));
%=============================================================================
V = sparse([1; 2; 3]);
S = sparse(1:3, 1:3, V, 3, 3);
asserts.istrue(issparse(S));
asserts.isequal(full(S), diag([1 2 3]));
%=============================================================================
A = sparse([1 0; 2 3; 0 4]);
c = max(max(abs(A))) / 1000;
S = sparse(1:3, 1:3, c * ones(3, 1), 3, 3);
asserts.istrue(issparse(S));
asserts.isapprox(full(S), 0.004 * eye(3, 3), 1e-12);
%=============================================================================
C = sparse(single([1 + 2i 0; 0 0]));
asserts.isequal(class(C), 'single');
asserts.istrue(issparse(C));
asserts.isequal(size(C), [2 2]);
asserts.isapprox(full(C), single([1 + 2i 0; 0 0]), 1e-6);
%=============================================================================
