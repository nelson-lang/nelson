%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = 31 .* 41;
REF = 1271;
assert_isequal(R, REF);
%=============================================================================
R = 41 .* 31;
REF = 1271;
assert_isequal(R, REF);
%=============================================================================
A = [11 0 31];
B = [21 31 71];
R = A.*B;
REF = [231 0 2201];
assert_isequal(R, REF);
%=============================================================================
A = [11 0 31; 51 31 81; 21 41 61];
B = [21 31 71; 91 11 51; 81 81 31];
R = A.*B;
REF = [   231.0000     0.0000  2201.0000;
4641.0000   341.0000  4131.0000;
1701.0000  3321.0000  1891.0000];
assert_isequal(R, REF);
%=============================================================================
R = [11 21 31 41] .* 2;
REF = [22 42 62 82];
assert_isequal(R, REF);
%=============================================================================
R = 2 .* [11 21 31 41];
REF = [22 42 62 82];
assert_isequal(R, REF);
%=============================================================================
assert_isequal([11 21;31 41;51 61] .* [11 21;31 41;51 61], [121 441;961 1681;2601 3721]);
assert_isequal([11 21;31 41;51 61i] .* [11 21;31 41;51 61], [121 441;961 1681;2601 3721i]);
assert_isequal([11 21;31 41;51 61] .* [11 21;31 41;51 61i], [121 441;961 1681;2601 3721i]);
assert_isequal([11 21;31 41;51 61i] .* [11 21;31 41;51 61i], [121 441;961 1681;2601 -3721]);
%=============================================================================
R = [11 21;31 41;51 61].*[11 21;31 41;51 61]-[121 441;961 1681;2601 3721];
REF = zeros(3, 2);
assert_isequal(R, REF);
%=============================================================================
R = [11 21;31 41;51 61i] .* [11 21;31 41;51 61] - [121 441;961 1681;2601 3721i];
REF = zeros(3, 2);
assert_isequal(R, REF);
%=============================================================================
R = [11 21;31 41;51 61].*[11 21;31 41;51 61i] - [121 441;961 1681;2601 3721i];
REF = zeros(3, 2);
assert_isequal(R, REF);
%=============================================================================
R = [11 21;31 41;51 61i].*[11 21;31 41;51 61i] - [121 441;961 1681;2601 -3721];
REF = zeros(3, 2);
assert_isequal(R, REF);
%=============================================================================
R = 31i .* 41;
REF = 1271i;
assert_isequal(R, REF);
%=============================================================================
R = 41 .* 31i;
REF = 1271i;
assert_isequal(R, REF);
%=============================================================================
R = [11 21 31 41] .* 2i;
REF = [22i 42i 62i 82i];
assert_isequal(R, REF);
%=============================================================================
R = 2i .* [11 21 31 41];
REF = [22i 42i 62i 82i];
assert_isequal(R, REF);
%=============================================================================
R = [] * [];
REF = [];
assert_isequal(R, REF);
%=============================================================================
R = ones(3, 9, 3) .* 34;
REF = 34 .* ones(3, 9, 3);
assert_isequal(R, REF);
%=============================================================================
R = single(41) .* single(31i);
REF = single(1271i);
assert_isequal(R, REF);
%=============================================================================
A = 1:2;
B = (1:3)';
R = A .* B;
REF = [   1     2;2     4;3     6];
assert_isequal(R, REF);
%=============================================================================
A = 1:2;
B = (1:3)';
R = B .* A;
REF = [   1     2;2     4;3     6];
assert_isequal(R, REF);
%=============================================================================
A = int32(3);
B = int32([1 2; 3 4]);
R = A .* B;
REF = int32([   3     6;9     12]);
assert_isequal(R, REF);
%=============================================================================
