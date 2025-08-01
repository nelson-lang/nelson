%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = 'no word, no bond, row on.';
B = flip(A);
assert_isequal(B, '.no wor ,dnob on ,drow on');
%=============================================================================
A = eye(2, 3);
B = flip(A, 1);
REF = [ 0     1     0;1     0     0];
assert_isequal(B, REF);
%=============================================================================
B = flip(A, 2);
REF = [ 0     0     1;0     1     0];
assert_isequal(B, REF);
%=============================================================================
B = flip(A, 3);
REF = [1     0     0;0     1     0];
assert_isequal(B, REF);
%=============================================================================
A = [];
A(1:2,1:2,1) = [5 6; 7 8];
A(1:2,1:2,2) = [1 2; 3 4];
B = flip(A, 1);
REF = [];
REF(1:2,1:2,1) = [7 8;5 6];
REF(1:2,1:2,2) = [3 4;1 2];
assert_isequal(B, REF);
%=============================================================================
A = [];
A(1:2,1:2,1) = [5 6; 7 8];
A(1:2,1:2,2) = [1 2; 3 4];
B = flip(A, 2);
REF = [];
REF(1:2,1:2,1) = [6 5;8 7];
REF(1:2,1:2,2) = [2 1;4 3];
assert_isequal(B, REF);
%=============================================================================
A = [];
A(1:2,1:2,1) = [5 6; 7 8];
A(1:2,1:2,2) = [1 2; 3 4];
B = flip(A, 3);
REF = [];
REF(1:2,1:2,1) = [1 2;3 4];
REF(1:2,1:2,2) = [5 6;7 8];
assert_isequal(B, REF);
%=============================================================================
A = [];
A(1:2,1:2,1) = [5 6; 7 8];
A(1:2,1:2,2) = [1 2; 3 4];
B = flip(A);
REF = flip(A, 1);
assert_isequal(B, REF);
%=============================================================================
