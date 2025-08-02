%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('sort'), -1);
assert_isequal(nargout('sort'), 2);
%=============================================================================
A = sort([1 NaN 3]);
REF = [1     3   NaN];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3], 'ascend');
REF = [1     3   NaN];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3], 'descend');
REF = [ NaN     3     1];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3 NaN], 'MissingPlacement', 'first');
REF = [NaN   NaN     1     3];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3 NaN], 'ascend', 'MissingPlacement', 'first');
REF = [NaN   NaN     1     3];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3 NaN], 'descend', 'MissingPlacement', 'first');
REF = [NaN   NaN     3     1];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3 NaN], 'MissingPlacement', 'last');
REF = [1     3   NaN   NaN];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3 NaN], 'ascend', 'MissingPlacement', 'last');
REF = [1     3   NaN   NaN];
assert_isequal(A, REF);
%=============================================================================
A = sort([1 NaN 3 NaN], 'descend', 'MissingPlacement', 'last');
REF = [3     1   NaN   NaN];
assert_isequal(A, REF);
%=============================================================================
A = [NaN 60 50; 70 NaN 40; 10 00 NaN];
R = sort(A, 2);
REF = [50     60    NaN;
40     70    NaN;
0     10    NaN];
assert_isequal(R, REF);
%=============================================================================
A = [NaN 60 50; 70 NaN 40; 10 00 NaN];
R = sort(A, 3);
assert_isequal(R, A);
%=============================================================================
A = [NaN 60 50; 70 NaN 40; 10 00 NaN];
R = sort(A, 2, 'descend');
REF = [NaN,    60,    50;   NaN,    70,    40;   NaN,    10,     0];
assert_isequal(R, REF);
%=============================================================================
S = sort(zeros(3, 0));
assert_isequal(S, zeros(3, 0));
[S, I] = sort(zeros(3, 0));
assert_isequal(S, zeros(3, 0));
assert_isequal(I, zeros(3, 0));
%=============================================================================
A = [9 0 -7 5 3 8 -10 4 2];
B = sort(A);
REF = [-10    -7     0     2     3     4     5     8     9];
assert_isequal(B, REF);
%=============================================================================
A = [9 0 -7 5 3 8 -10 4 2];
[B, I] = sort(A);
REF  = [-10    -7     0     2     3     4     5     8     9];
REF_I = [7     3     2     9     5     8     4     6     1];
assert_isequal(I, REF_I);
assert_isequal(B, REF);
%=============================================================================
A = [9 0 -7 5 3 8 -10 4 2];
R = sort(A, 1);
REF = [ 9     0    -7     5     3     8   -10     4     2];
assert_isequal(R, REF);
%=============================================================================
A = [9 0 -7 5 3 8 -10 4 2];
R = sort(A, 2);
REF = [-10    -7     0     2     3     4     5     8     9];
assert_isequal(R, REF);
%=============================================================================
A = [9 0 -7 5 3 8 -10 4 2]';
R = sort(A, 1);
REF = [ -10; -7; 0; 2; 3; 4; 5; 8; 9];
assert_isequal(R, REF);
%=============================================================================
A = [9 0 -7 5 3 8 -10 4 2]';
R = sort(A, 2);
REF = [ 9; 0; -7; 5; 3; 8; -10; 4; 2];
assert_isequal(R, REF);
%=============================================================================
A = [NaN 1 3 NaN 2];
R = sort(A, 'ascend', 'MissingPlacement', 'first');
REF = [ NaN   NaN     1     2     3];
assert_isequal(R, REF);
%=============================================================================
A = [NaN 1 3 NaN 2];
R = sort(A, 'descend', 'MissingPlacement', 'first');
REF = [ NaN   NaN     3     2     1];
assert_isequal(R, REF);
%=============================================================================
A = [NaN 1 3 NaN 2];
R = sort(A, 'ascend', 'MissingPlacement', 'last');
REF = [  1     2     3  NaN   NaN ];
assert_isequal(R, REF);
%=============================================================================
A = [NaN 1 3 NaN 2];
R = sort(A, 'descend', 'MissingPlacement', 'last');
REF = [ 3     2     1  NaN   NaN    ];
assert_isequal(R, REF);
%=============================================================================
A = [NaN 1 3 NaN 2];
[R, I] = sort(A, 'descend', 'MissingPlacement', 'last');
REF = [ 3     2     1  NaN   NaN    ];
REF_I = [  3     5     2     1     4];
assert_isequal(R, REF);
assert_isequal(I, REF_I);
%=============================================================================
A = [3 6 5; 7 -2 4; 1 0 -9];
[R, I] = sort(A, 'descend', 'MissingPlacement', 'last');
REF_R = [     7     6     5;     3     0     4;     1    -2    -9];
REF_I = [2     1     1;    1     3     2;     3     2     3];
assert_isequal(R, REF_R);
assert_isequal(I, REF_I);
%=============================================================================
