%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('unique'), -1);
assert_isequal(nargout('unique'), -1);
%=============================================================================
A = [9 2 9 5];
R = unique(A);
REF = [2 5 9];
assert_isequal(R, REF);
%=============================================================================
A = [5 5 NaN NaN];
R = unique(A);
REF = [5 NaN NaN];
assert_isequal(R, REF);
%=============================================================================
A = [9 2 9 5];
[C, ia, ic] = unique(A);
REF_ia = [2; 4; 1];
REF_ic = [3; 1; 3; 2];
assert_isequal(ia, REF_ia);
assert_isequal(ic, REF_ic);
%=============================================================================
A = magic(6);
R = unique(A, 'rows');
REF =   [ 3    32     7    21    23    25;
4    36    29    13    18    11;
8    28    33    17    10    15;
30     5    34    12    14    16;
31     9     2    22    27    20;
35     1     6    26    19    24];
assert_isequal(R, REF);
%=============================================================================
A = magic(6);
[R, ia] = unique(A, 'rows');
REF =   [ 3    32     7    21    23    25;
4    36    29    13    18    11;
8    28    33    17    10    15;
30     5    34    12    14    16;
31     9     2    22    27    20;
35     1     6    26    19    24];
assert_isequal(R, REF);
ia_REF = [2;6;4;5;3;1];
assert_isequal(ia, ia_REF);
%=============================================================================
A = magic(6);
[R, ia, ic] = unique(A, 'rows');
REF =   [ 3    32     7    21    23    25;
4    36    29    13    18    11;
8    28    33    17    10    15;
30     5    34    12    14    16;
31     9     2    22    27    20;
35     1     6    26    19    24];
assert_isequal(R, REF);
ia_REF = [2;6;4;5;3;1];
assert_isequal(ia, ia_REF);
ic_REF = [6;1;5;3;4;2];
assert_isequal(ic, ic_REF);
%=============================================================================
a=magic(18);
b=reshape(a, 2, 3, []);
assert_checkerror("unique(b, 'rows')", _('''rows'' mode only works for 2D matrix.'))
%=============================================================================
A = [10,20,30; 10,20,30];
[C,ia,ic] = unique(A, 'rows');
C_REF = [10   20   30];
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
ic_REF = [1; 1];
assert_isequal(ic, ic_REF);
%=============================================================================
A = [10,20,30; 10,20,30];
[C,ia] = unique(A, 'rows');
C_REF = [10   20   30];
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
%=============================================================================
A = [10,20,30; 10,20,30];
[C] = unique(A, 'rows');
C_REF = [10   20   30];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([1 NaN NaN 2]);
C_REF = [1     2   NaN   NaN];
assert_isequal(C, C_REF);
ia_REF = [1;   4;   2;   3];
assert_isequal(ia, ia_REF);
ic_REF = [1;   3;   4;   2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([1 NaN NaN 2]);
C_REF = [1     2   NaN   NaN];
assert_isequal(C, C_REF);
ia_REF = [1;   4;   2;   3];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([1 NaN NaN 2]);
C_REF = [1     2   NaN   NaN];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10 10 20; 10 20 10; 10 10 20]);
C_REF = [10 ; 20];
assert_isequal(C, C_REF);
ia_REF = [1; 5];
assert_isequal(ia, ia_REF);
ic_REF = [1;   1;   1;   1;   2;   1;   2;   1;   2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10 10 20; 10 20 10; 10 10 20]);
C_REF = [10 ; 20];
assert_isequal(C, C_REF);
ia_REF = [1; 5];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10 10 20; 10 20 10; 10 10 20]);
C_REF = [10 ; 20];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10 10 20; 10 0 10; 10 10 20], 'rows');
C_REF = [10    0   10;    10   10   20];
assert_isequal(C, C_REF);
ia_REF = [2; 1];
assert_isequal(ia, ia_REF);
ic_REF = [ 2; 1; 2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10 10 20; 10 0 10; 10 10 20], 'rows');
C_REF = [10    0   10;    10   10   20];
assert_isequal(C, C_REF);
ia_REF = [2; 1];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10 10 20; 10 0 10; 10 10 20], 'rows');
C_REF = [10    0   10;    10   10   20];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([]);
C_REF = zeros(0, 1);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
ic_REF = zeros(0, 1);
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([]);
C_REF =  zeros(0, 1);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([]);
C_REF =  zeros(0, 1);
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(10);
C_REF = 10;
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
ic_REF = 1;
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(10);
C_REF = 10;
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(10);
C_REF = 10;
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10, 10, 20, 30, 30, 30, 40]);
C_REF = [10   20   30   40];
assert_isequal(C, C_REF);
ia_REF = [1;   3;   4;   7];
assert_isequal(ia, ia_REF);
ic_REF = [1;   1;   2;   3;   3;   3;   4];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10, 10, 20, 30, 30, 30, 40]);
C_REF = [10   20   30   40];
assert_isequal(C, C_REF);
ia_REF = [1;   3;   4;   7];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10, 10, 20, 30, 30, 30, 40]);
C_REF = [10   20   30   40];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10 20]);
C_REF = [10   20];
assert_isequal(C, C_REF);
ia_REF = [1;   2];
assert_isequal(ia, ia_REF);
ic_REF = [1; 2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10 20]);
C_REF = [10   20];
assert_isequal(C, C_REF);
ia_REF = [1;   2];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10 20]);
C_REF = [10   20];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10;20]);
C_REF = [10;  20];
assert_isequal(C, C_REF);
ia_REF = [1;   2];
assert_isequal(ia, ia_REF);
ic_REF = [1; 2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10;20]);
C_REF = [10;  20];
assert_isequal(C, C_REF);
ia_REF = [1;   2];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10;20]);
C_REF = [10;  20];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10, NaN, Inf, NaN, Inf]);
C_REF = [10   Inf   NaN   NaN];
assert_isequal(C, C_REF);
ia_REF = [1;    3;   2;   4];
assert_isequal(ia, ia_REF);
ic_REF = [1;   3;   2;   4;   2];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10, NaN, Inf, NaN, Inf]);
C_REF = [10   Inf   NaN   NaN];
assert_isequal(C, C_REF);
ia_REF = [1;    3;   2;   4];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10, NaN, Inf, NaN, Inf]);
C_REF = [10   Inf   NaN   NaN];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10,20,20,30,20,40], 'rows');
C_REF = [10    20    20    30    20    40];
assert_isequal(C, C_REF);
ia_REF =  1;
assert_isequal(ia, ia_REF);
ic_REF =  1;
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10,20,20,30,20,40], 'rows');
C_REF = [10    20    20    30    20    40];
assert_isequal(C, C_REF);
ia_REF =  1;
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10,20,20,30,20,40], 'rows');
C_REF = [10    20    20    30    20    40];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10,20,20,30,20,40]);
C_REF = [10    20    30    40];
assert_isequal(C, C_REF);
ia_REF = [1;     2;     4;     6];
assert_isequal(ia, ia_REF);
ic_REF = [     1;     2;     2;     3;     2;     4];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10,20,20,30,20,40]);
C_REF = [10    20    30    40];
assert_isequal(C, C_REF);
ia_REF = [1;     2;     4;     6];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10,20,20,30,20,40]);
C_REF = [10    20    30    40];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10,20,20,30,20,40]', 'rows');
C_REF = [10;    20;    30;    40];
assert_isequal(C, C_REF);
ia_REF = [1;      2;      4;      6];
assert_isequal(ia, ia_REF);
ic_REF = [1;      2;     2;     3;     2;     4];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10,20,20,30,20,40]', 'rows');
C_REF = [10;    20;    30;    40];
assert_isequal(C, C_REF);
ia_REF = [1;      2;      4;      6];
assert_isequal(ia, ia_REF);
%=============================================================================
[C, ia] = unique([10,20,20,30,20,40]', 'rows');
C_REF = [10;    20;    30;    40];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique([10,20,20,30,20,40]');
C_REF = [10;     20;    30;    40];
assert_isequal(C, C_REF);
ia_REF = [1;      2;     4;     6];
assert_isequal(ia, ia_REF);
ic_REF = [ 1;     2;     2;     3;     2;     4];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique([10,20,20,30,20,40]');
C_REF = [10;     20;    30;    40];
assert_isequal(C, C_REF);
ia_REF = [1;      2;     4;     6];
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique([10,20,20,30,20,40]');
C_REF = [10;     20;    30;    40];
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(zeros(13,0), 'rows');
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
ic_REF = [  1;     1;     1;     1;     1;     1;     1;     1;     1;     1;     1;     1;     1];
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(zeros(13,0), 'rows');
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(zeros(13,0), 'rows');
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(zeros(13,0));
C_REF = zeros(0, 1);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
ic_REF = zeros(0, 1);
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(zeros(13,0));
C_REF = zeros(0, 1);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(zeros(13,0));
C_REF = zeros(0, 1);
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(zeros(1,0));
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
ic_REF = zeros(0, 1);
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(zeros(1,0));
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
ia_REF = zeros(0, 1);
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(zeros(1,0));
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(zeros(1,0), 'rows');
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
ic_REF = 1;
assert_isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(zeros(1,0), 'rows');
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
ia_REF = 1;
assert_isequal(ia, ia_REF);
%=============================================================================
[C] = unique(zeros(1,0), 'rows');
C_REF = zeros(1, 0);
assert_isequal(C, C_REF);
%=============================================================================
