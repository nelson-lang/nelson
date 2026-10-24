%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('sprank'), 1);
asserts.isequal(nargout('sprank'), 1);
%=============================================================================
A = sparse([1 1; 1 1]);
asserts.isequal(rank(full(A)), 1);
asserts.isequal(sprank(A), 2);
%=============================================================================
B = sparse([1 1; 1 1; 0 0]);
asserts.isequal(sprank(B), 2);
%=============================================================================
C = sparse([0 1 0; 1 0 1; 0 1 0]);
asserts.isequal(sprank(C), 2);
%=============================================================================
D = sparse([1 0 0 0; 0 0 2 0; 0 0 0 3]);
asserts.isequal(sprank(D), 3);
%=============================================================================
E = sparse([1 1 0; 0 1 1; 1 0 1]);
asserts.isequal(sprank(E), 3);
%=============================================================================
F = sparse([1 + 2i 0; 0 0]);
asserts.isequal(sprank(F), 1);
%=============================================================================
S = sparse(single([1 0 1; 0 2 0; 3 0 0]));
asserts.isequal(sprank(S), 3);
%=============================================================================
C = sparse(single([1 + 2i 0; 0 0]));
asserts.isequal(sprank(C), 1);
%=============================================================================
SCZ = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
asserts.isequal(sprank(SCZ), 1);
%=============================================================================
asserts.isequal(sprank(sparse([NaN 0; 0 0])), 1);
asserts.isequal(sprank(sparse([Inf 0; 0 0])), 1);
asserts.isequal(sprank(sparse([1i 0; 0 0])), 1);
asserts.isequal(sprank(single([NaN 0; 0 0])), 1);
%=============================================================================
FC = [1 + 1i 0 2; 0 0 3];
asserts.isequal(sprank(FC), 2);
%=============================================================================
G = [1 1; 1 1];
asserts.isequal(sprank(G), 2);
%=============================================================================
H = single([0 1; 2 0]);
asserts.isequal(sprank(H), 2);
%=============================================================================
L = sparse(logical([true true false; false true true]));
asserts.isequal(sprank(L), 2);
%=============================================================================
DL = logical([1 1 0 0 0;
              0 1 1 0 0;
              0 0 0 1 1;
              1 0 0 0 0]);
asserts.isequal(sprank(DL), 4);
%=============================================================================
P = sparse([1 0 0 1 0;
            0 1 0 0 1;
            1 0 0 1 0;
            0 0 1 0 0]);
asserts.isequal(sprank(P), 4);
%=============================================================================
Z = sparse(zeros(3, 4));
asserts.isequal(sprank(Z), 0);
%=============================================================================
ZS = sparse(zeros(2, 2));
ZS(1, 2) = 1;
ZS(1, 2) = 0;
asserts.isequal(sprank(ZS), 0);
%=============================================================================
asserts.isequal(sprank(sparse(0, 3)), 0);
asserts.isequal(sprank(sparse(3, 0)), 0);
asserts.isequal(sprank(sparse([1 0 0 1; 0 1 0 0])), 2);
asserts.isequal(sprank(sparse([0 1 0 1; 1 0 0 0; 0 0 1 0])), 3);
asserts.isequal(sprank(sparse([1 0 0; 0 0 1])), 2);
%=============================================================================
R1 = sparse([1 1 0 0; 1 1 0 0; 0 0 1 1]);
asserts.isequal(rank(full(R1)), 2);
asserts.isequal(sprank(R1), 3);
%=============================================================================
R2 = sparse([1 1 0; 1 1 0; 0 0 1; 0 0 1]);
asserts.isequal(rank(full(R2)), 2);
asserts.isequal(sprank(R2), 3);
%=============================================================================
asserts.isequal(sprank(sparse([])), 0);
asserts.checkerror('sprank(ones(2, 2, 2))', sprintf(_('%s must be a 2-D matrix.'), 'Value'));
asserts.checkerror('sprank({1})', _('A numeric or logical 2-D matrix expected.'));
asserts.checkerror('sprank(''abc'')', _('A numeric or logical 2-D matrix expected.'));
asserts.checkerror('sprank(uint8([1 0; 0 1]))', _('A floating-point or logical 2-D matrix expected.'));
%=============================================================================
