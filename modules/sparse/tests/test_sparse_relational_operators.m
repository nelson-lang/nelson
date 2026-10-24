%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([1 0 2; 0 3 0]);
B = sparse([1 4 0; 0 0 0]);
%=============================================================================
R = A == B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false false; true false true])));
%=============================================================================
R = A ~= B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true true; false true false])));
%=============================================================================
R = A < B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; false false false])));
%=============================================================================
R = A <= B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true true false; true false true])));
%=============================================================================
R = A > B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false true; false true false])));
%=============================================================================
R = A >= B;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; true true true])));
%=============================================================================
C = sparse([0 0 3; 0 2 0; 0 0 0]);
D = sparse([0 1 0; 0 2 0; 4 0 0]);
R = C == D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false false; true true true; false true true])));
%=============================================================================
R = C ~= D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true true; false false false; true false false])));
%=============================================================================
R = C < D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; false false false; true false false])));
%=============================================================================
R = C <= D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true true false; true true true; true true true])));
%=============================================================================
R = C > D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false true; false false false; false false false])));
%=============================================================================
R = C >= D;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; true true true; false true true])));
%=============================================================================
Bfull = [1 4 0; 0 0 0];
R = A == Bfull;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false false; true false true])));
%=============================================================================
R = Bfull ~= A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true true; false true false])));
%=============================================================================
R = A < Bfull;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; false false false])));
%=============================================================================
R = Bfull <= A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; true true true])));
%=============================================================================
R = A > 0;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; false true false])));
%=============================================================================
R = A >= 0;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true true true; true true true])));
%=============================================================================
R = A == 0;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; true false true])));
%=============================================================================
R = 0 < A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; false true false])));
%=============================================================================
R = A <= 0;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; true false true])));
%=============================================================================
R = 0 <= A;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true true true; true true true])));
%=============================================================================
R = A == 1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false false; false false false])));
%=============================================================================
R = A ~= 1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true true; true true true])));
%=============================================================================
R = A < 1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; true false true])));
%=============================================================================
R = A <= 1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true true false; true false true])));
%=============================================================================
R = A > 1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false true; false true false])));
%=============================================================================
R = A >= 1;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; false true false])));
%=============================================================================
L = sparse(logical([true false; false true]));
R = L == true;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false; false true])));
%=============================================================================
R = L ~= false;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false; false true])));
%=============================================================================
X = sparse([1 + 2i 0; 0 3 - 4i]);
Y = [1 + 2i 5; 0 0];
R = X == Y;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false; true false])));
%=============================================================================
R = X ~= Y;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true; false true])));
%=============================================================================
AS = sparse(single([1 0 2; 0 3 0]));
BS = sparse(single([1 4 0; 0 0 0]));
R = AS == BS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false false; true false true])));
%=============================================================================
R = AS < BS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true false; false false false])));
%=============================================================================
BSFULL = single([1 4 0; 0 0 0]);
R = BSFULL ~= AS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true true; false true false])));
%=============================================================================
R = AS >= single(1);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false true; false true false])));
%=============================================================================
XS = sparse(single([1 + 2i 0; 0 3 - 4i]));
YS = single([1 + 2i 5; 0 0]);
R = XS == YS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([true false; true false])));
%=============================================================================
R = XS ~= YS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false true; false true])));
%=============================================================================
ZS = sparse(single([2 + 0.1i 0; 0 1 + 0.1i]));
R = XS > ZS;
asserts.istrue(issparse(R));
asserts.isequal(R, sparse(logical([false false; false true])));
%=============================================================================
