%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('spaugment'), -1);
asserts.isequal(nargout('spaugment'), 1);
%=============================================================================
A = sparse([1 0; 2 3; 0 4]);
S = spaugment(A);
REF = [0.004 0 0 1 0; 0 0.004 0 2 3; 0 0 0.004 0 4; 1 2 0 0 0; 0 3 4 0 0];
asserts.istrue(issparse(S));
asserts.isequal(size(S), [5 5]);
asserts.isapprox(full(S), REF, 1e-12);
%=============================================================================
S = spaugment(A, 2);
REF = [2 0 0 1 0; 0 2 0 2 3; 0 0 2 0 4; 1 2 0 0 0; 0 3 4 0 0];
asserts.istrue(issparse(S));
asserts.isapprox(full(S), REF, 1e-12);
%=============================================================================
S = spaugment(full(A), 2);
asserts.istrue(issparse(S));
asserts.isapprox(full(S), REF, 1e-12);
%=============================================================================
S = spaugment(A, [3 9]);
asserts.isapprox(full(S(1:3, 1:3)), 3 * eye(3), 1e-12);
%=============================================================================
S = spaugment(A, true);
asserts.istrue(issparse(S));
asserts.isapprox(full(S(1:3, 1:3)), eye(3, 3), 1e-12);
%=============================================================================
S = spaugment(A, false);
asserts.istrue(issparse(S));
asserts.isapprox(full(S(1:3, 1:3)), zeros(3, 3), 1e-12);
%=============================================================================
S = spaugment(A, single(3));
asserts.istrue(issparse(S));
asserts.isapprox(full(S(1:3, 1:3)), 3 * eye(3, 3), 1e-12);
%=============================================================================
S = spaugment(A, 'a');
asserts.istrue(issparse(S));
asserts.isapprox(full(S(1:3, 1:3)), 97 * eye(3, 3), 1e-12);
%=============================================================================
C = sparse([1 + 1i 0; 0 2]);
SC = spaugment(C, 0.5);
asserts.istrue(issparse(SC));
asserts.isapprox(full(SC(1:2, 1:2)), 0.5 * eye(2), 1e-12);
asserts.isapprox(full(SC(1:2, 3:4)), full(C), 1e-12);
asserts.isapprox(full(SC(3:4, 1:2)), full(C'), 1e-12);
%=============================================================================
SK = spaugment(sparse(eye(2, 2)), 1 + 2i);
asserts.istrue(issparse(SK));
asserts.isapprox(full(SK(1:2, 1:2)), (1 + 2i) * eye(2), 1e-12);
%=============================================================================
RM = sparse([1 0 2; 0 -3 0]);
SRM = spaugment(RM, 1 + 2i);
REF_RM = [1 + 2i 0 1 0 2;
    0 1 + 2i 0 -3 0;
    1 0 0 0 0;
    0 -3 0 0 0;
    2 0 0 0 0];
asserts.istrue(issparse(SRM));
asserts.isequal(size(SRM), [5 5]);
asserts.isequal(nnz(SRM), 8);
asserts.isapprox(full(SRM), REF_RM, 1e-12);
%=============================================================================
SL = spaugment(sparse(logical([true false; false true])), 2);
REF_LOGICAL = [2 0 1 0; 0 2 0 1; 1 0 0 0; 0 1 0 0];
asserts.istrue(issparse(SL));
asserts.isapprox(full(SL), REF_LOGICAL, 1e-12);
%=============================================================================
CS = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
SCS = spaugment(CS, single(1));
REF_CS = single([1 0 0 0; 0 1 0 2i; 0 0 0 0; 0 -2i 0 0]);
asserts.istrue(issparse(SCS));
asserts.isequal(class(SCS), 'single');
asserts.isequal(size(SCS), [4 4]);
asserts.isequal(nnz(SCS), 4);
asserts.isapprox(full(SCS), REF_CS, 1e-6);
%=============================================================================
Z = spaugment(sparse(zeros(2, 3)));
asserts.istrue(issparse(Z));
asserts.isequal(size(Z), [5 5]);
asserts.isequal(nnz(Z), 0);
%=============================================================================
asserts.checkerror('spaugment(ones(2, 2, 2))', _('A numeric or logical 2-D matrix expected.'));
asserts.checkerror('spaugment({1})', _('A numeric or logical 2-D matrix expected.'));
asserts.checkerror('spaugment(sparse([]))', _('A must be a non-empty matrix.'));
asserts.checkerror('spaugment(sparse(0, 3))', _('A must be a non-empty matrix.'));
asserts.checkerror('spaugment(sparse(2, 0))', _('A must be a non-empty matrix.'));
asserts.checkerror('spaugment(A, {})', _('C must be a non-empty numeric, logical, or character array.'));
asserts.checkerror('spaugment(A, ones(1, 1, 2))', _('C must be a 2-D array.'));
%=============================================================================
