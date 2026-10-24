%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(class(prod(single(3i), 'default')), 'single');
asserts.isequal(prod(single(3i)), single(3i));
%=============================================================================
asserts.isequal(class(prod(single(3i), 'double')), 'double');
%=============================================================================
asserts.isequal(class(prod(true, 'native')), 'logical');
asserts.isequal(prod(true, 'native'), true);
%=============================================================================
asserts.isequal(class(prod(true, 'default')), 'double');
asserts.isequal(prod(true, 'default'), 1);
%=============================================================================
asserts.isequal(class(prod(true, 'double')), 'double');
asserts.isequal(prod(true, 'double'), 1);
%=============================================================================
asserts.checkerror('prod(''A'')', _('PROD does not work with char inputs.'))
%=============================================================================
M = [8 1 6;
  3 5 7;
  4 9 2];
R = prod(M);
REF = [96 45 84];
asserts.isequal(R, REF);
%=============================================================================
R = prod(M, 2);
REF = [48; 105; 72];
asserts.isequal(R, REF);
%=============================================================================
M = [8 1 6;
  3 5 7;
  4 9 2];
R = prod(M, 4);
REF = M;
asserts.isequal(R, REF);
%=============================================================================
M = [10:30:70; 20:30:80; 30:30:90];
R = prod(M);
REF = [6000 120000 504000];
asserts.isequal(R, REF);
%=============================================================================
M = [true, false; true, true];
R = prod(M);
REF = [1 0];
asserts.isequal(R, REF);
%=============================================================================
M = [1:3:7; 2:3:8; 3:3:9];
M(:, :, 2) = [10:3:16; 11:3:17; 12:3:18];
R = prod(M, 3);
REF = [10 52 112; 22 70 136; 36 90 162];
asserts.isequal(R, REF);
%=============================================================================
M = single([12 15 18; 13 16 19; 14 17 20]);
R = prod(M, 2, 'double');
REF = [3240; 3952; 4760];
asserts.isequal(R, REF);
asserts.isequal(class(R), 'double');
%=============================================================================
M = uint8([10:30:70; 20:30:80; 30:30:90]);
R = prod(M, 'native');
REF = uint8([255 255 255]);
asserts.isequal(R, REF);
%=============================================================================
M = [10 3 2 4 NaN 30 NaN 20];
R = prod(M, 'omitnan');
REF = 144000;
asserts.isequal(R, REF);
%=============================================================================
R = prod(zeros(0, 0));
REF = 1;
asserts.isequal(R, REF);
%=============================================================================
R = prod(zeros(0, 1));
REF = 1;
asserts.isequal(R, REF);
%=============================================================================
R = prod([intmax('uint64'), intmin('uint64'), intmax('uint64')]);
REF = 0;
asserts.isequal(R, REF);
%=============================================================================
A = int32(1:10);
R = prod(A, 'native');
REF = int32(3628800);
asserts.isequal(R, REF);
%=============================================================================
M = [1 NaN 3; 4 5 6];
asserts.isequal(prod(M(:), 'omitnan'), 360);
asserts.isequal(prod(M, 2, 'omitnan'), [3; 120]);
asserts.isequal(prod(zeros(0, 3), 1), [1 1 1]);
%=============================================================================
columnValues = int32([1; 2; 3]);
asserts.isequal(prod(columnValues, 2), double(columnValues));
asserts.isequal(prod(single(columnValues), 2), single(columnValues));
sparseColumn = sparse([1; 0; 3]);
R = prod(sparseColumn, 2);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1; 0; 3]);
%=============================================================================
% 'all' option
assert_isequal(prod([1 2 3; 4 5 6], 'all'), 720);
assert_isequal(prod(int32([2 3; 4 5]), 'all'), 120);
assert_isequal(prod([1 NaN; 3 4], 'all', 'omitnan'), 12);
assert_isequal(prod(5, 'all'), 5);
%=============================================================================
% vector of dimensions (vecdim)
A = reshape(1:24, 2, 3, 4);
assert_isequal(prod(A, [1 2 3]), prod(1:24));
assert_isequal(prod([1 2; 3 4], [1 2]), 24);
%=============================================================================
