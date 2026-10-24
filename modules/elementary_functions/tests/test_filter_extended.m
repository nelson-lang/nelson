%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
y = filter([1 1], 1, [1 2 3 4]);
REF = [1 3 5 7];
asserts.isequal(y, REF);
%=============================================================================
[y, zf] = filter(2, 1, uint8([1 2 3]));
asserts.isequal(y, [2 4 6]);
asserts.isequal(class(y), 'double');
asserts.isequal(zf, []);
%=============================================================================
y = filter(1, [1 -0.5], [1 0 0 0]);
REF = [1 0.5 0.25 0.125];
asserts.isapprox(y, REF, 1e-12);
%=============================================================================
[y, zf] = filter([1 1], 1, [1 2 3], 10);
REF = [11 3 5];
ZF_REF = 3;
asserts.isequal(y, REF);
asserts.isequal(zf, ZF_REF);
%=============================================================================
x = [1 2 3; 4 5 6];
[y, zf] = filter([1 1], 1, x, [], 2);
REF = [1 3 5; 4 9 11];
ZF_REF = [3 6];
asserts.isequal(y, REF);
asserts.isequal(size(zf), [1 2]);
asserts.isequal(zf, ZF_REF);
%=============================================================================
[y, zf] = filter([1 2 3], [1 -0.5], x, zeros(2, 2), 2);
REF = [1 4.5 12.25; 4 15 35.5];
ZF_REF = [18.125 44.75; 9 18];
asserts.isapprox(y, REF, 1e-12);
asserts.isequal(size(zf), [2 2]);
asserts.isapprox(zf, ZF_REF, 1e-12);
%=============================================================================
[y, zf] = filter([1 2 3], [1 -0.5], x, zeros(2, 3), 1);
REF = [1 2 3; 6.5 10 13.5];
ZF_REF = [14.25 21 27.75; 12 15 18];
asserts.isapprox(y, REF, 1e-12);
asserts.isequal(size(zf), [2 3]);
asserts.isapprox(zf, ZF_REF, 1e-12);
%=============================================================================
[y, zf] = filter([1 + i 2], [1 -0.25], [1 2i 3 4i], 0.5i);
REF = [1 + 1.5i 0.25 + 2.375i 3.0625 + 7.59375i 2.765625 + 5.8984375i];
ZF_REF = 0.69140625 + 9.474609375i;
asserts.isapprox(y, REF, 1e-12);
asserts.isapprox(zf, ZF_REF, 1e-12);
%=============================================================================
x3 = reshape(1:24, [2 3 4]);
[y, zf] = filter([1 2], 1, x3, [], 3);
REF = cat(3, [1 3 5; 2 4 6], [9 15 21; 12 18 24], ...
  [27 33 39; 30 36 42], [45 51 57; 48 54 60]);
ZF_REF = cat(3, [38 40], [42 44], [46 48]);
asserts.isequal(y, REF);
asserts.isequal(size(zf), [1 2 3]);
asserts.isequal(zf, ZF_REF);
%=============================================================================
asserts.checkerror('filter([1 1], 1, sparse([1 0 2]))', _('Sparse inputs are not supported.'));
asserts.checkerror('filter(sparse([1 1]), 1, [1 2 3])', _('Sparse inputs are not supported.'));
asserts.checkerror('filter([1 1], 1, [1 2 3], sparse(0))', _('Sparse inputs are not supported.'));
%=============================================================================
