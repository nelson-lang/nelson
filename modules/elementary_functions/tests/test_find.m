%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
asserts.isequal(nargin('find'), 3);
asserts.isequal(nargout('find'), 3);
%=============================================================================
R = find([1 0 2; 0 1 1; 0 0 4]);
REF = [1; 5; 7; 8; 9];
asserts.isequal(R, REF);
%=============================================================================
R = find(~[1 0 2; 0 1 1; 0 0 4]);
REF = [2; 3; 4; 6];
asserts.isequal(R, REF);
%=============================================================================
X = magic(6);
X(1:2:end) = 0;
R = find(X, 4, 'last');
REF = [30; 32; 34; 36];
asserts.isequal(R, REF);
%=============================================================================
R = find([0 2 0 3], Inf);
asserts.isequal(R, [2 4]);
R = find([0 2 0 3], Inf, 'LAST');
asserts.isequal(R, [2 4]);
R = find([0 2 0 3], 1, 'FIRST');
asserts.isequal(R, 2);
%=============================================================================
X = magic(6);
X(1:2:end) = 0;
[R, C, V] = find(X, 4, 'last');
R_REF = [6; 2; 4; 6];
C_REF = [5; 6; 6; 6];
V_REF = [18; 25; 15; 11];
asserts.isequal(R, R_REF);
asserts.isequal(C, C_REF);
asserts.isequal(V, V_REF);
%=============================================================================
R = 1:2:20;
R = find(R == 13);
REF = 7;
asserts.isequal(R, REF);
%=============================================================================
R = 0:0.1:1;
R = find(R == 0.3);
REF = ones(1, 0);
asserts.isequal(R, REF);
%=============================================================================
X = [18 3 1 11; 8 10 11 3; 9 14 6 1; 4 3 15 21];
[R, C] = find(X > 0 & X < 10, 3);
R_REF = [2; 3; 4];
C_REF = [1; 1; 1];
asserts.isequal(R, R_REF);
asserts.isequal(C, C_REF);
%=============================================================================
X = [3 2 0; -5 0 7; 0 0 1];
[R, C, V] = find(X);
R_REF = [1; 2; 1; 2; 3];
C_REF = [1; 1; 2; 3; 3];
V_REF = [3; -5; 2; 7; 1];
asserts.isequal(R, R_REF);
asserts.isequal(C, C_REF);
asserts.isequal(V, V_REF);
%=============================================================================
X = zeros(4, 2, 3);
X([1 12 19 21]) = 1;
[R, C] = find(X);
R_REF = [1; 4; 3; 1];
C_REF = [1; 3; 5; 6];
asserts.isequal(R, R_REF);
asserts.isequal(C, C_REF);
%=============================================================================
[findRows, findColumns, findValues] = find([0 5; 6 0]);
asserts.isequal(findRows, [2; 1]);
asserts.isequal(findColumns, [1; 2]);
asserts.isequal(findValues, [6; 5]);
%=============================================================================
S = sparse([0 2 + 3i; 4 0; 0 5]);
[findRows, findColumns, findValues] = find(S);
asserts.isequal(findRows, [2; 1; 3]);
asserts.isequal(findColumns, [1; 2; 2]);
asserts.isapprox(findValues, [4; 2 + 3i; 5], 1e-12);
asserts.isequal(find(S, 2), [2; 4]);
asserts.isequal(find(S, 2, 'last'), [4; 6]);
asserts.isequal(find(S, Inf, 'LAST'), [2; 4; 6]);
[findRows, findColumns, findValues] = find(S, 2, 'last');
asserts.isequal(findRows, [1; 3]);
asserts.isequal(findColumns, [2; 2]);
asserts.isapprox(findValues, [2 + 3i; 5], 1e-12);
%=============================================================================
ZC = sparse([1 2 1 2], [1 1 2 2], [0 -0 complex(0, 0) complex(0, 2)], 2, 2, 4);
asserts.isequal(find(ZC), 4);
[findRows, findColumns, findValues] = find(ZC);
asserts.isequal(findRows, 2);
asserts.isequal(findColumns, 2);
asserts.isapprox(findValues, complex(0, 2), 1e-12);
%=============================================================================
ZCS = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
asserts.isequal(find(ZCS), 4);
[findRows, findColumns, findValues] = find(ZCS);
asserts.isequal(findRows, 2);
asserts.isequal(findColumns, 2);
asserts.isequal(class(findValues), 'single');
asserts.isapprox(findValues, single(complex(0, 2)), 1e-6);
%=============================================================================
C = sparse(single([1 + 2i 0; 0 0]));
asserts.isequal(find(C), 1);
[findRows, findColumns, findValues] = find(C);
asserts.isequal(findRows, 1);
asserts.isequal(findColumns, 1);
asserts.isequal(class(findValues), 'single');
asserts.isapprox(findValues, single(1 + 2i), 1e-6);
%=============================================================================
L = sparse(logical([true false true; false true false]));
[findRows, findColumns, findValues] = find(L);
asserts.isequal(findRows, [1; 2; 1]);
asserts.isequal(findColumns, [1; 2; 3]);
asserts.isequal(class(findValues), 'logical');
asserts.isequal(findValues, [true; true; true]);
asserts.isequal(find(L, 2, 'last'), [4; 5]);
[findRows, findColumns, findValues] = find(L, 2, 'last');
asserts.isequal(findRows, [2; 1]);
asserts.isequal(findColumns, [2; 3]);
asserts.isequal(class(findValues), 'logical');
asserts.isequal(findValues, [true; true]);
%=============================================================================
SR = sparse([0 5 0 6]);
findLinear = find(SR);
asserts.isequal(findLinear, [2 4]);
asserts.isequal(size(findLinear), [1 2]);
[findRows, findColumns, findValues] = find(SR);
asserts.isequal(findRows, [1 1]);
asserts.isequal(size(findRows), [1 2]);
asserts.isequal(findColumns, [2 4]);
asserts.isequal(size(findColumns), [1 2]);
asserts.isequal(findValues, [5 6]);
asserts.isequal(size(findValues), [1 2]);
[findRows, findColumns, findValues] = find(SR, 1, 'last');
asserts.isequal(findRows, 1);
asserts.isequal(findColumns, 4);
asserts.isequal(findValues, 6);
%=============================================================================
SZ = sparse([1 0; 0 2]);
SZ(1, 2) = 5;
SZ(1, 2) = 0;
asserts.isequal(find(SZ), [1; 4]);
[findRows, findColumns, findValues] = find(SZ);
asserts.isequal(findRows, [1; 2]);
asserts.isequal(findColumns, [1; 2]);
asserts.isequal(findValues, [1; 2]);
asserts.isequal(find(SZ, 1, 'last'), 4);
%=============================================================================
Y = zeros(2, 2, 2);
Y([1 4 7]) = [5 6 7];
[findRows, findColumns, findValues] = find(Y);
asserts.isequal(findRows, [1; 2; 1]);
asserts.isequal(findColumns, [1; 2; 4]);
asserts.isequal(findValues, [5; 6; 7]);
%=============================================================================
