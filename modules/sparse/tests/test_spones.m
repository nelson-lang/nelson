%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = [1, 0; 3, 4];
R = spones(S);
asserts.istrue(issparse(R));
[I, J, V] = IJV(R);
I_REF = [1; 2; 2];
asserts.isequal(I, I_REF);
J_REF = [1; 1; 2];
asserts.isequal(J, J_REF);
V_REF = [1; 1; 1];
asserts.isequal(V, V_REF);
%=============================================================================
S = sparse([1, 0; 3, 4]);
R = spones(S);
asserts.istrue(issparse(R));
[I, J, V] = IJV(R);
I_REF = [1; 2; 2];
asserts.isequal(I, I_REF);
J_REF = [1; 1; 2];
asserts.isequal(J, J_REF);
V_REF = [1; 1; 1];
asserts.isequal(V, V_REF);
%=============================================================================
SZ = sparse([1 0; 0 2]);
SZ(1, 2) = 5;
SZ(1, 2) = 0;
R = spones(SZ);
asserts.istrue(issparse(R));
asserts.isequal(full(R), eye(2, 2));
asserts.isequal(nnz(R), 2);
%=============================================================================
C = sparse([0 2 + 3i; 4 0; 0 5]);
R = spones(C);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
asserts.isequal(full(R), [0 1; 1 0; 0 1]);
asserts.isequal(nnz(R), nnz(C));
%=============================================================================
S = sparse(single([1 0 1; 0 2 0; 3 0 0]));
R = spones(S);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([1 0 1; 0 1 0; 1 0 0]));
asserts.isequal(nnz(R), nnz(S));
%=============================================================================
C = sparse(single([1 + 2i 0; 0 0]));
R = spones(C);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([1 0; 0 0]));
asserts.isequal(nnz(R), nnz(C));
%=============================================================================
R = spones([NaN 0; Inf -Inf]);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 0; 1 1]);
asserts.isequal(nnz(R), 3);
%=============================================================================
R = spones(sparse([NaN 0; 0 Inf]));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 0; 0 1]);
asserts.isequal(nnz(R), 2);
%=============================================================================
ZC = sparse([1 2 1 2], [1 1 2 2], [0 -0 complex(0, 0) complex(0, 2)], 2, 2, 4);
R = spones(ZC);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [0 0; 0 1]);
asserts.isequal(nnz(R), 1);
%=============================================================================
ZCS = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
R = spones(ZCS);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([0 0; 0 1]));
asserts.isequal(nnz(R), 1);
%=============================================================================
R = spones('a b');
asserts.istrue(issparse(R));
asserts.isequal(size(R), [1 3]);
asserts.isequal(full(R), [1 1 1]);
asserts.isequal(nnz(R), 3);
%=============================================================================
L = sparse(logical([true false; false true]));
R = spones(L);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
asserts.isequal(full(R), eye(2, 2));
asserts.isequal(nnz(R), 2);
%=============================================================================
R = spones(logical([true false; false true]));
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'double');
asserts.isequal(full(R), eye(2, 2));
asserts.isequal(nnz(R), 2);
%=============================================================================
E = sparse(zeros(0, 3));
R = spones(E);
asserts.istrue(issparse(R));
asserts.isequal(size(R), [0 3]);
asserts.isequal(nnz(R), 0);
%=============================================================================
R = spones(sparse(zeros(1, 0)));
asserts.istrue(issparse(R));
asserts.isequal(size(R), [1 0]);
asserts.isequal(nnz(R), 0);
%=============================================================================
R = spones(sparse(zeros(0, 1)));
asserts.istrue(issparse(R));
asserts.isequal(size(R), [0 1]);
asserts.isequal(nnz(R), 0);
%=============================================================================
asserts.checkerror('spones(zeros(1, 1, 2))', _('ND-sparse arrays are not supported.'));
%=============================================================================
