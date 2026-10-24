%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(bitcmp(uint8(5)), uint8(250));
asserts.isequal(bitcmp(uint16(1)), uint16(65534));
asserts.isequal(bitcmp(uint8([0 255])), uint8([255 0]));
asserts.isequal(bitcmp(int8(5)), int8(-6));
asserts.isequal(bitcmp(uint32(0)), uint32(4294967295));
asserts.isequal(class(bitcmp(uint64(0))), 'uint64');
%=============================================================================
