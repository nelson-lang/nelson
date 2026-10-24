%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(class(sum(single(3i), 'default')), 'single');
asserts.isequal(sum(single(3i)), single(3i));
%=============================================================================
asserts.isequal(class(sum(single(3i), 'double')), 'double');
%=============================================================================
asserts.isequal(class(sum(true, 'native')), 'logical');
asserts.isequal(sum(true, 'native'), true);
%=============================================================================
asserts.isequal(class(sum(true, 'default')), 'double');
asserts.isequal(sum(true, 'default'), 1);
%=============================================================================
asserts.isequal(class(sum(true, 'double')), 'double');
asserts.isequal(sum(true, 'double'), 1);
%=============================================================================
asserts.isequal(sum('A'), 65)
asserts.isequal(class(sum('A')), 'double')
asserts.isequal(sum('AB'), 131)
%=============================================================================
M = [8 1 6;
  3 5 7;
  4 9 2];
R = sum(M);
REF = [15 15 15];
asserts.isequal(R, REF);
%=============================================================================
R = sum(M, 2);
REF = [15; 15; 15];
asserts.isequal(R, REF);
%=============================================================================
asserts.isequal(sum([0; 2] .^ 2, 2), [0; 4]);
asserts.isequal(sum(single([0; 2]) .^ 2, 2), single([0; 4]));
asserts.isequal(sum(int32([0; 2]) .^ 2, 2), [0; 4]);
%=============================================================================
M = [8 1 6;
  3 5 7;
  4 9 2];
R = sum(M, 4);
REF = M;
asserts.isequal(R, REF);
%=============================================================================
M = [10:30:70; 20:30:80; 30:30:90];
R = sum(M);
REF = [60 150 240];
asserts.isequal(R, REF);
%=============================================================================
M = [true, false; true, true];
R = sum(M);
REF = [2 1];
asserts.isequal(R, REF);
%=============================================================================
M = [1:3:7; 2:3:8; 3:3:9];
M(:, :, 2) = [10:3:16; 11:3:17; 12:3:18];
R = sum(M, 3);
REF = [11 17 23; 13 19 25; 15 21 27];
asserts.isequal(R, REF);
%=============================================================================
M = single([12 15 18; 13 16 19; 14 17 20]);
R = sum(M, 2, 'double');
REF = [45; 48; 51];
asserts.isequal(R, REF);
asserts.isequal(class(R), 'double');
%=============================================================================
M = uint8([10:30:70; 20:30:80; 30:30:90]);
R = sum(M, 'native');
REF = uint8([60 150 240]);
asserts.isequal(R, REF);
%=============================================================================
M = [10 3 2 4 NaN 30 NaN 20];
R = sum(M, 'omitnan');
REF = 69;
asserts.isequal(R, REF);
%=============================================================================
M = [1 2; 3 4];
R = sum(M, 'all');
REF = 10;
asserts.isequal(R, REF);
%=============================================================================
M = [1 NaN; 3 4];
R = sum(M, 'all', 'omitnan');
REF = 8;
asserts.isequal(R, REF);
%=============================================================================
M = uint8([1 2; 3 4]);
R = sum(M, 'all', 'native');
REF = uint8(10);
asserts.isequal(R, REF);
%=============================================================================
M = single([1 2; 3 4]);
R = sum(M, 'all', 'double', 'includenan');
REF = 10;
asserts.isequal(R, REF);
asserts.isequal(class(R), 'double');
%=============================================================================
R = sum(zeros(0, 0));
REF = 0;
asserts.isequal(R, REF);
%=============================================================================
R = sum(zeros(0, 1));
REF = 0;
asserts.isequal(R, REF);
%=============================================================================
R = sum(zeros(3, 0, 3));
REF = zeros(1, 0, 3);
asserts.isequal(R, REF);
%=============================================================================
R = sum([intmax('uint64'), intmin('uint64'), intmax('uint64')]);
REF = 3.689348814741910e+19;
asserts.isapprox(R, REF, eps);
%=============================================================================
A = int32(1:10);
R = sum(A, 'native');
REF = int32(55);
asserts.isequal(R, REF);
%=============================================================================
M = [1 NaN 3; 4 5 6];
asserts.isequal(sum(M, 'all', 'omitnan'), 19);
asserts.isequal(sum(M, 2, 'omitnan'), [4; 15]);
asserts.isequal(sum(zeros(0, 3), 1), [0 0 0]);
asserts.isequal(class(sum(single([1 2; 3 4]), 1)), 'single');
%=============================================================================
S = sparse(single([1 0; 2 3]));
R = sum(S, 1);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([3 3]));
R = sum(S, 'all');
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single(6));
%=============================================================================
C = sparse(single([1 + 2i 0; 2 3 - 1i]));
R = sum(C, 2);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([1 + 2i; 5 - 1i]));
%=============================================================================
columnValues = int32([1; 2; 3]);
asserts.isequal(sum(columnValues, 2), double(columnValues));
sparseColumn = sparse([1; 0; 3]);
R = sum(sparseColumn, 2);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1; 0; 3]);
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sum');
asserts.checkerror('sum({1})', msg);
%=============================================================================
% vector of dimensions (vecdim)
A = reshape(1:24, 2, 3, 4);
assert_isequal(sum(A, [1 2]), reshape([21 57 93 129], 1, 1, 4));
assert_isequal(sum(A, [1 3]), [84 100 116]);
assert_isequal(sum(A, [1 2 3]), 300);
assert_isequal(sum([1 2 3; 4 5 6], [1 2]), 21);
assert_isequal(sum([1 NaN; 3 4], [1 2], 'omitnan'), 8);
%=============================================================================
