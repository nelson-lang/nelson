%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(bitset(uint8(0), 3), uint8(4));
asserts.isequal(bitset(uint8(255), 1, 0), uint8(254));
asserts.isequal(bitset(uint8(4), [1 2 3]), uint8([5 6 4]));
asserts.isequal(bitset(12, 3), 12);
asserts.isequal(bitset(uint8([1 2 4]), 1), uint8([1 3 5]));
asserts.isequal(bitset(uint8(0), 3, 1), uint8(4));
asserts.isequal(bitset(uint8([0 0 0]), 1, [1 0 1]), uint8([1 0 1]));
asserts.isequal(bitset(int32(0), 5), int32(16));
asserts.isequal(class(bitset(uint16(0), 4)), 'uint16');
%=============================================================================
