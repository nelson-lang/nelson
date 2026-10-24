%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(idivide(int8(-128),int8(-1)), int8(127));
asserts.isequal(idivide(int8(5),int8(0)), int8(127));
asserts.isequal(idivide(int8(-5),int8(0)), int8(-128));
asserts.isequal(idivide(uint8(5),uint8(0)), uint8(255));
asserts.isequal(idivide(int8(-128),int8(-1),'floor'), int8(127));
asserts.isequal(idivide(int8(-128),int8(-1),'ceil'), int8(127));
asserts.isequal(idivide(int8(-128),int8(-1),'round'), int8(127));
asserts.isequal(idivide(int16(-32768),int16(-1)), int16(32767));
asserts.isequal(idivide(int32(-2147483647-1),int32(-1)), int32(2147483647));
asserts.isequal(idivide(intmin('int64'),int64(-1)), (int64(9223372036)*int64(1000000000) + int64(854775807)));
asserts.isequal(idivide(intmin('int64'),int64(-1),'floor'), (int64(9223372036)*int64(1000000000) + int64(854775807)));
asserts.isequal(idivide(intmin('int64'),int64(-1),'round'), (int64(9223372036)*int64(1000000000) + int64(854775807)));
asserts.isequal(idivide(intmin('int64'),int64(-1),'ceil'), (int64(9223372036)*int64(1000000000) + int64(854775807)));
asserts.isequal(idivide(int8([1 2 3]),int8(0)), [int8(127) int8(127) int8(127)]);
asserts.isequal(idivide(int8([1 -2 0]),int8(0)), [int8(127) int8(-128) int8(0)]);
asserts.isequal(idivide(int8(5),int8(0),'floor'), int8(127));
asserts.isequal(idivide(int8(-5),int8(0),'floor'), int8(-128));
asserts.isequal(idivide(int8(-5),int8(0),'ceil'), int8(-128));
asserts.isequal(idivide(int8(-5),int8(0),'round'), int8(-128));
asserts.isequal(idivide(int8(-5),int8(0),'fix'), int8(-128));
asserts.isequal(idivide(uint8(5),uint8(0),'floor'), uint8(255));
asserts.isequal(idivide(uint8(5),uint8(0),'ceil'), uint8(255));
asserts.isequal(idivide(uint8(5),uint8(0),'round'), uint8(255));
asserts.isequal(idivide(int64(5),int64(0)), (int64(9223372036)*int64(1000000000) + int64(854775807)));
asserts.isequal(idivide(int64(-5),int64(0)), (-int64(9223372036)*int64(1000000000) - int64(854775808)));
asserts.isequal(idivide(int16(5),int16(0)), int16(32767));
asserts.isequal(idivide(int16(-5),int16(0)), int16(-32768));
asserts.isequal(idivide(uint16(5),uint16(0)), uint16(65535));
asserts.isequal(idivide(int32(5),int32(0)), int32(2147483647));
asserts.isequal(idivide(int32(-5),int32(0)), int32(-2147483648));
asserts.isequal(idivide(uint32(5),uint32(0)), uint32(4294967295));
asserts.isequal(idivide(int8([10 20]),int8([3 0])), [int8(3) int8(127)]);
asserts.isequal(idivide(int8([5 -5 0]),int8(0)), [int8(127) int8(-128) int8(0)]);
%=============================================================================
