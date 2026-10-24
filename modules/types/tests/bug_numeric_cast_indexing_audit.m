%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% numeric casts saturate at type bounds; scalar indexing rejects fractional
% and negative indices; linear growth of a matrix errors (2026-07 audit).
%=============================================================================
% saturation at the upper bounds (used to wrap around)
asserts.isequal(int64(2 ^ 63), intmax('int64'));
asserts.isequal(uint64(2 ^ 64), intmax('uint64'));
asserts.isequal(int32(single(2 ^ 31)), intmax('int32'));
asserts.isequal(int64(-2 ^ 63), intmin('int64'));
asserts.isequal(int8(200), int8(127));
asserts.isequal(uint8(-5), uint8(0));
%=============================================================================
% uint16 -> logical must not truncate to 8 bits
asserts.isequal(xor(uint16(256), false), true);
%=============================================================================
% fractional scalar index must error
A = [10 20 30];
asserts.checkerror('A(2.5)', _('index must either be real positive integers or logicals.'));
%=============================================================================
% negative integer index: proper index error (used to attempt a huge allocation)
asserts.checkerror('A(int8(-2)) = 5;', _('Zero or negative index encountered.'));
%=============================================================================
% growing a 2-D matrix through a linear index is ambiguous
B = magic(3);
asserts.checkerror('B(15) = 1;', _('Attempt to grow array along ambiguous dimension.'));
%=============================================================================
% empties, scalars and vectors still grow
C = [];
C(5) = 1;
asserts.isequal(C, [0 0 0 0 1]);
D = 1;
D(3) = 2;
asserts.isequal(D, [1 0 2]);
E = [1 2 3];
E(5) = 9;
asserts.isequal(E, [1 2 3 0 9]);
%=============================================================================
