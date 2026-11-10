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
%=============================================================================
M = single([4 1 0; 0 5 2; 7 0 6; 0 3 0]);
S = sparse(M);
d = diag(S);
asserts.istrue(issparse(d));
asserts.isequal(class(d), 'single');
asserts.isequal(size(d), [3 1]);
asserts.isequal(full(d), single([4; 5; 6]));
asserts.isequal(full(diag(S, -2)), single([7; 3]));
asserts.isequal(full(diag(S, 1)), single([1; 2]));
d = diag(S, -1);
asserts.isequal(size(d), [3 1]);
asserts.isequal(nnz(d), 0);
d = diag(S, 3);
asserts.isequal(size(d), [0 1]);
asserts.isequal(class(d), 'single');
%=============================================================================
SC = sparse(M + 2i * (M > 4));
d = diag(SC);
asserts.isequal(class(d), 'single');
asserts.isfalse(isreal(d));
asserts.isequal(full(d), single([4; 5 + 2i; 6 + 2i]));
d = diag(SC, 1);
asserts.istrue(isreal(d));
asserts.isequal(full(d), single([1; 2]));
asserts.istrue(isreal(diag(SC, 3)));
%=============================================================================
v = sparse(single([1 0 3]));
D = diag(v);
asserts.istrue(issparse(D));
asserts.isequal(class(D), 'single');
asserts.isequal(full(D), single(diag([1 0 3])));
D = diag(v, -2);
asserts.isequal(size(D), [5 5]);
asserts.isequal(full(D), single(diag([1 0 3], -2)));
D = diag(sparse(single([1; 0; 3 + 1i])), 1);
asserts.isequal(size(D), [4 4]);
asserts.isfalse(isreal(D));
asserts.isequal(full(D), single(diag([1; 0; 3 + 1i], 1)));
%=============================================================================
asserts.isequal(size(diag(sparse(single(zeros(0, 3))))), [0 1]);
asserts.isequal(size(diag(sparse(single(zeros(1, 0))), 1)), [1 1]);
D = diag(sparse(single(5)), 1);
asserts.isequal(full(D), single([0 5; 0 0]));
%=============================================================================
% a diagonal taken from a complex matrix is real when its values are real
asserts.istrue(isreal(diag([1+1i 2; 3 4], 1)));
asserts.istrue(isreal(diag(single([1+1i 2; 3 4]), 3)));
asserts.istrue(isreal(diag(sparse([1 2; 3 4i]), -3)));
asserts.isfalse(isreal(diag([1+1i 2; 3 4])));
asserts.isfalse(isreal(diag([1+1i 2 3], 1)));
asserts.isfalse(isreal(diag(sparse([1+1i 2 3]), 1)));
%=============================================================================
[~, ~, ~, ~, D] = lu(sparse(single([4 1; 1 5])));
asserts.isequal(full(diag(D)), single([5; 6]));
%=============================================================================
