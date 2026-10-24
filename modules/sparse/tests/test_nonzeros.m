%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('nonzeros'), 1);
asserts.isequal(nargout('nonzeros'), 1);
%=============================================================================
A = [1 0 2; 0 3 0];
v = nonzeros(A);
asserts.isfalse(issparse(v));
asserts.isequal(v, [1; 3; 2]);
%=============================================================================
S = sparse(A);
v = nonzeros(S);
asserts.isfalse(issparse(v));
asserts.isequal(v, [1; 3; 2]);
%=============================================================================
v = nonzeros(sparse([0 4 0 5]));
asserts.isequal(v, [4; 5]);
%=============================================================================
v = nonzeros(sparse(zeros(0, 3)));
asserts.isequal(size(v), [0 1]);
asserts.isequal(class(v), 'double');
%=============================================================================
v = nonzeros(sparse(zeros(2, 2)));
asserts.isequal(size(v), [0 1]);
asserts.isequal(class(v), 'double');
%=============================================================================
SZ = sparse([1 0; 0 2]);
SZ(1, 2) = 5;
SZ(1, 2) = 0;
v = nonzeros(SZ);
asserts.isequal(v, [1; 2]);
%=============================================================================
C = sparse([1 + 2i 0; 0 3 - 4i]);
v = nonzeros(C);
asserts.isfalse(issparse(v));
asserts.isapprox(v, [1 + 2i; 3 - 4i], 1e-12);
%=============================================================================
ZC = sparse([1 2 1 2], [1 1 2 2], [0 -0 complex(0, 0) complex(0, 2)], 2, 2, 4);
v = nonzeros(ZC);
asserts.isfalse(issparse(v));
asserts.isapprox(v, complex(0, 2), 1e-12);
%=============================================================================
ZCS = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
v = nonzeros(ZCS);
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'single');
asserts.isapprox(v, single(complex(0, 2)), 1e-6);
%=============================================================================
L = sparse(logical([true false; false true]));
v = nonzeros(L);
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'logical');
asserts.isequal(v, [true; true]);
%=============================================================================
v = nonzeros(logical([true false; false true]));
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'logical');
asserts.isequal(v, [true; true]);
%=============================================================================
v = nonzeros(uint8([1 0; 0 2]));
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'uint8');
asserts.isequal(v, uint8([1; 2]));
%=============================================================================
v = nonzeros(int16([0 -3; 4 0]));
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'int16');
asserts.isequal(v, int16([4; -3]));
%=============================================================================
v = nonzeros(single([0 1.5; 2.5 0]));
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'single');
asserts.isapprox(v, single([2.5; 1.5]), 1e-6);
%=============================================================================
S = sparse(single([1 0; 0 2]));
v = nonzeros(S);
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'single');
asserts.isequal(v, single([1; 2]));
%=============================================================================
C = sparse(single([1 + 2i 0; 0 0]));
v = nonzeros(C);
asserts.isfalse(issparse(v));
asserts.isequal(class(v), 'single');
asserts.isapprox(v, single(1 + 2i), 1e-6);
%=============================================================================
v = nonzeros(reshape([0 1 2 0], [1 2 2]));
asserts.isequal(size(v), [2 1]);
asserts.isequal(v, [1; 2]);
%=============================================================================
v = nonzeros('a b');
asserts.isequal(size(v), [3 1]);
asserts.isequal(class(v), 'char');
asserts.isequal(v, ['a'; ' '; 'b']);
%=============================================================================
asserts.checkerror('nonzeros({1})', _('A numeric, logical, or character array expected.'));
%=============================================================================
