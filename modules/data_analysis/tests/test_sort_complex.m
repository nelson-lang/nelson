%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [10+2i, 30+i, i, 0, -i];
B = sort(A,'ComparisonMethod','auto');
REF = [  0,   -i,  i,  10+2i,  30+i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i 30+i i 0 -i];
B = sort(A,'ComparisonMethod','real');
REF = [ -i,  0, i,  10 + 2i,  30 + i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i 30+i i 0 -i];
B = sort(A,'ComparisonMethod','abs');
REF = [  0,   -i,  i,  10+2i,  30+i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i, 30+i, i, 0, -i];
B = sort(A, 'descend', 'ComparisonMethod','auto');
REF = [30.0000 + 1.0000i  10.0000 + 2.0000i   0.0000 + 1.0000i   0.0000 - 1.0000i   0.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i 30+i i 0 -i];
B = sort(A, 'descend', 'ComparisonMethod','real');
REF = [30.0000 + 1.0000i  10.0000 + 2.0000i   0.0000 + 1.0000i   0.0000 + 0.0000i   0.0000 - 1.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i 30+i i 0 -i];
B = sort(A, 'descend', 'ComparisonMethod','abs');
REF = [30.0000 + 1.0000i  10.0000 + 2.0000i   0.0000 + 1.0000i   0.0000 - 1.0000i   0.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [-4 6 3+4i 1+i 0];
B = sort(A,'ComparisonMethod','auto');
REF = [0.0000 + 0.0000i   1.0000 + 1.0000i  -4.0000 + 0.0000i   3.0000 + 4.0000i   6.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [-4 6 3+4i 1+i 0];
B = sort(A,'ComparisonMethod','abs');
REF = [   0.0000 + 0.0000i   1.0000 + 1.0000i  -4.0000 + 0.0000i   3.0000 + 4.0000i   6.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
B = sort(A,'descend','ComparisonMethod','abs');
REF = [ 6.0000 + 0.0000i   3.0000 + 4.0000i  -4.0000 + 0.0000i   1.0000 + 1.0000i   0.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i, 30+i; i,  -i];
B = sort(A,'ComparisonMethod','auto');
REF = [  0.0000 + 1.0000i,   0.0000 - 1.0000i;
10.0000 + 2.0000i,  30.0000 + 1.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i, 30+i; i,  -i];
B = sort(A,'ComparisonMethod','real');
REF = [0.0000 + 1.0000i,   0.0000 - 1.0000i;
10.0000 + 2.0000i,  30.0000 + 1.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [10+2i 30+i; i  -i];
B = sort(A,'ComparisonMethod','abs');
REF = [0.0000 + 1.0000i,   0.0000 - 1.0000i;
10.0000 + 2.0000i,  30.0000 + 1.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [-4 6 3+4i complex(NaN, 1) 0];
B = sort(A,'ComparisonMethod','real', 'MissingPlacement', 'first');
REF = [NaN + 1.0000i  -4.0000 + 0.0000i   0.0000 + 0.0000i   3.0000 + 4.0000i   6.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [-4 6 3+4i; complex(1, NaN) 0 complex(NaN, 1)];
B = sort(A,'ComparisonMethod','real', 'MissingPlacement', 'first');
REF = [   complex(1, NaN)   0.0000 + 0.0000i     complex(NaN, 1);
-4.0000 + 0.0000i   6.0000 + 0.0000i   3.0000 + 4.0000i];
assert_isequal(B, REF);
%=============================================================================
X = [3  1  5 ; 2 9 8];
Y = [2  4  1 ; 4 1 3];
A = X + Y*i;
B1 = sort(A,'ComparisonMethod','real');
REF_1 = [   2.0000 + 4.0000i   1.0000 + 4.0000i   5.0000 + 1.0000i;
3.0000 + 2.0000i   9.0000 + 1.0000i   8.0000 + 3.0000i];
assert_isequal(B1, REF_1);
B2 = sort(A,'ComparisonMethod','abs');
REF_2 = [3.0000 + 2.0000i   1.0000 + 4.0000i   5.0000 + 1.0000i;
2.0000 + 4.0000i   9.0000 + 1.0000i   8.0000 + 3.0000i];
assert_isequal(B2, REF_2);
%=============================================================================
A = [10+2i, 30+i, i, 0, -i];
[B, I] = sort(A, 'descend', 'ComparisonMethod','auto');
REF = [30.0000 + 1.0000i  10.0000 + 2.0000i   0.0000 + 1.0000i   0.0000 - 1.0000i   0.0000 + 0.0000i];
REF_I = [2     1     3     5     4];
assert_isequal(B, REF);
assert_isequal(I, REF_I);
assert_isequal(A, [10+2i, 30+i, i, 0, -i]);
%=============================================================================
A = [10+2i 30+i i 0 -i];
[B, I] = sort(A, 'descend', 'ComparisonMethod','real');
REF = [30.0000 + 1.0000i  10.0000 + 2.0000i   0.0000 + 1.0000i   0.0000 + 0.0000i   0.0000 - 1.0000i];
REF_I = [ 2     1     3     4     5];
assert_isequal(B, REF);
assert_isequal(I, REF_I);
%=============================================================================
A = [10+2i 30+i i 0 -i];
[B, I] = sort(A, 'descend', 'ComparisonMethod','abs');
REF = [30.0000 + 1.0000i  10.0000 + 2.0000i   0.0000 + 1.0000i   0.0000 - 1.0000i   0.0000 + 0.0000i];
REF_I = [2     1     3     5     4];
assert_isequal(B, REF);
assert_isequal(I, REF_I);
%=============================================================================
A = [10+2i, 30+i; i,  -i];
[B, I] = sort(A,'ComparisonMethod','auto');
REF = [     2     2;     1     1];
assert_isequal(A, [10+2i, 30+i; i,  -i]);
assert_isequal(I, REF);
%=============================================================================
A = [10+2i, 30+i; i,  -i];
[B, I] = sort(A,'ComparisonMethod','real');
REF = [     2     2;     1     1];
assert_isequal(A, [10+2i, 30+i; i,  -i]);
assert_isequal(I, REF);
%=============================================================================
A = [10+2i 30+i; i  -i];
[B, I] = sort(A,'ComparisonMethod','abs');
REF = [     2     2;     1     1];
assert_isequal(A, [10+2i, 30+i; i,  -i]);
assert_isequal(I, REF);
%=============================================================================
A = [complex(NaN,-4), complex(2,NaN), complex(NaN, -2)];
B = sort(A,'ComparisonMethod','real');
REF = [complex(NaN,-4), complex(2,NaN), complex(NaN, -2)];
assert_isequal(B, REF);
%=============================================================================
A = [complex(2,NaN), complex(-2,NaN)];
B = sort(A,'ComparisonMethod','real');
REF = [complex(2,NaN), complex(-2,NaN)];
assert_isequal(B, REF);
%=============================================================================
A = [complex(-2,NaN), complex(2,NaN)];
B = sort(A,'ComparisonMethod','real');
REF = [complex(-2,NaN), complex(2,NaN)];
assert_isequal(B, REF);
%=============================================================================
A = [complex(-2, 5), complex(-2, 4)];
B = sort(A,'ComparisonMethod','real');
REF = [  -2.0000 + 4.0000i  -2.0000 + 5.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [complex(-2,NaN), complex(2,NaN)];
[B, I] = sort(A,'ComparisonMethod','real');
REF = [complex(-2,NaN), complex(2,NaN)];
assert_isequal(B, REF);
REF_I = [1 2];
assert_isequal(I, REF_I);
%=============================================================================
A = [complex(-2, 5), complex(-2, 4)];
[B, I] = sort(A,'ComparisonMethod','real');
REF = [  -2.0000 + 4.0000i  -2.0000 + 5.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [complex(NaN,-4), 2, complex(2, NaN), 1, complex(NaN, 0), complex(NaN, NaN), 3, complex(0, NaN), -1, complex(-1,NaN)];
B = sort(A,'ComparisonMethod','real');
REF = [-1   1   2   3    complex(NaN, -4) complex(2, NaN) complex(NaN, 0) complex(NaN, NaN) complex(0, NaN) complex(-1, NaN)];
assert_isequal(B, REF);
C = [complex(NaN,-4), 2, complex(2, NaN), 1, complex(NaN, 0), complex(NaN, NaN), 3, complex(0, NaN), -1, complex(-1,NaN)];
assert_isequal(A, C);
%=============================================================================
A = [3, complex(0, NaN), -1, complex(-1,NaN)];
B = sort(A,'ComparisonMethod','real');
REF = [  -1, 3, complex(0, NaN),  complex(-1,NaN)];
assert_isequal(B, REF);
%=============================================================================
A = [complex(NaN,-4), 2, complex(2, NaN), 1, complex(NaN, NaN), 3, complex(0, NaN), -1, complex(-1,NaN)];
C = [complex(NaN,-4), 2, complex(2, NaN), 1, complex(NaN, NaN), 3, complex(0, NaN), -1, complex(-1,NaN)];
[B, I] = sort(A);
REF_I = [4     8     2     6     1     3     5     7     9];
assert_isequal(I, REF_I);
assert_isequal(A, C);
%=============================================================================
A = [complex(NaN,-4), 2, complex(2, NaN), 1, complex(NaN, NaN), 3, complex(0, NaN), -1, complex(-1,NaN)];
C = [complex(NaN,-4), 2, complex(2, NaN), 1, complex(NaN, NaN), 3, complex(0, NaN), -1, complex(-1,NaN)];
[B, I] = sort(A,'ComparisonMethod','real');
REF = [-1   1   2   3     complex(NaN, -4)   complex(2, NaN) complex(NaN, NaN) complex(0, NaN)  complex(-1, NaN)];
assert_isequal(B, REF);
REF_I  = [8     4     2     6     1     3     5     7     9];
assert_isequal(I, REF_I);
assert_isequal(A, C);
%=============================================================================
A = [-4 6 3+4i 1+i 0];
B = sort(A,'ComparisonMethod','real');
REF = [  -4.0000 + 0.0000i   0.0000 + 0.0000i   1.0000 + 1.0000i   3.0000 + 4.0000i   6.0000 + 0.0000i];
assert_isequal(B, REF);
%=============================================================================
A = [-4 6 3+4i 1+i 0];
[B, I] = sort(A,'ComparisonMethod','real');
REF = [  -4.0000 + 0.0000i   0.0000 + 0.0000i   1.0000 + 1.0000i   3.0000 + 4.0000i   6.0000 + 0.0000i];
REF_I= [1     5     4     3     2];
assert_isequal(B, REF);
assert_isequal(I, REF_I);
%=============================================================================
A = [complex(NaN,-4), 2;
complex(2, NaN), 1;
complex(NaN, 0), complex(NaN, NaN);
3, complex(0, NaN);
-1, complex(-1,NaN)];
B = sort(A,'ComparisonMethod','real');
REF = [complex(-1,0), 1;
3, 2;
complex(NaN, -4), complex(NaN, NaN);
complex(2, NaN),   complex(0, NaN);
complex(NaN, 0),  complex(-1,NaN)];
assert_isequal(B, REF);
%=============================================================================
A = [complex(NaN,-4), 2;
complex(2, NaN), 1;
complex(NaN, 0), complex(NaN, NaN);
3, complex(0, NaN);
-1, complex(-1,NaN)];
[B, I] = sort(A,'ComparisonMethod','real');
REF = [complex(-1,0), 1;
3, 2;
complex(NaN, -4), complex(NaN, NaN);
complex(2, NaN),   complex(0, NaN);
complex(NaN, 0),  complex(-1,NaN)];
assert_isequal(B, REF);
REF_I = [     5     2;
4     1;
1     3;
2     4;
3     5];
assert_isequal(I, REF_I);
%=============================================================================
