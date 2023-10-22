%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(3.22f32, single(3.22))
assert_isequal(3.22f64, double(3.22))
assert_isequal(3.22f32i, single(3.22i))
assert_isequal(3.22f64i, double(3.22i))
%=============================================================================
assert_isequal(9223372036854775808i64, intmax('int64'))
assert_isequal(9223372036854775807i64, intmax('int64'))
assert_isequal(-9223372036854775808i64, intmin('int64'))
assert_isequal(-9223372036854775809i64, intmin('int64'))
%=============================================================================
assert_isequal(18446744073709551616u64, intmax('uint64'))
assert_isequal(18446744073709551615u64, intmax('uint64'))
assert_isequal(0u64, intmin('uint64'))
assert_checkerror('-18446744073709551616u64', ...
   _('Lexical error ''Malformed unsigned integer constant with unary operator ''-''.'''))
%=============================================================================
assert_isequal(9223372036854775808i32, intmax('int32'))
assert_isequal(9223372036854775807i32, intmax('int32'))
assert_isequal(-9223372036854775808i32, intmin('int32'))
assert_isequal(-9223372036854775809i32, intmin('int32'))
%=============================================================================
assert_isequal(42949672950u32, intmax('uint32'))
assert_isequal(4294967295u32, intmax('uint32'))
assert_isequal(0u32, intmin('uint32'))
assert_checkerror('-2u32', ...
   _('Lexical error ''Malformed unsigned integer constant with unary operator ''-''.'''))
%=============================================================================
assert_isequal(32768i16, intmax('int16'))
assert_isequal(32767i16, intmax('int16'))
assert_isequal(-32768i16, intmin('int16'))
assert_isequal(-32769i16, intmin('int16'))
%=============================================================================
assert_isequal(42949672950u16, intmax('uint16'))
assert_isequal(4294967295u16, intmax('uint16'))
assert_isequal(0u16, intmin('uint16'))
assert_checkerror('-2u16', ...
   _('Lexical error ''Malformed unsigned integer constant with unary operator ''-''.'''))
%=============================================================================
assert_isequal(127i8, intmax('int8'))
assert_isequal(127i8, intmax('int8'))
assert_isequal(-128i8, intmin('int8'))
assert_isequal(-128i8, intmin('int8'))
%=============================================================================
assert_isequal(255u8, intmax('uint8'))
assert_isequal(256u8, intmax('uint8'))
assert_isequal(0u8, intmin('uint8'))
assert_checkerror('-2u8', ...
   _('Lexical error ''Malformed unsigned integer constant with unary operator ''-''.'''))
%=============================================================================
