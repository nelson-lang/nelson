%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A bare integer literal passed directly to int64/uint64 is converted from its
% exact decimal text, not from a rounded double. Integers in (2^53, 2^63] and
% (2^53, 2^64] therefore stay exact.
%=============================================================================
asserts.isequal(mat2str(int64(9007199254740993)), '9007199254740993');
asserts.isequal(mat2str(uint64(9007199254740993)), '9007199254740993');
asserts.istrue(int64(9007199254740993) - int64(9007199254740992) == int64(1));
asserts.istrue(int64(9007199254740993) == int64(9007199254740993));
% The right-hand literal here is an ordinary (rounded) double, so the exact
% int64 does not compare equal to it, as the integer comparison operators
% require.
asserts.istrue(~(int64(9007199254740993) == 9007199254740993));
%=============================================================================
% The signed/unsigned extremes are preserved exactly.
asserts.isequal(int64(9223372036854775807), intmax('int64'));
asserts.isequal(int64(-9223372036854775808), intmin('int64'));
asserts.isequal(uint64(18446744073709551615), intmax('uint64'));
asserts.isequal(mat2str(uint64(9223372036854775809)), '9223372036854775809');
%=============================================================================
% A leading unary minus is folded so the magnitude stays exact.
asserts.isequal(mat2str(int64(-9007199254740993)), '-9007199254740993');
%=============================================================================
% Saturation still applies to an out-of-range literal.
asserts.isequal(uint64(20000000000000000000), intmax('uint64'));
asserts.isequal(int64(-9223372036854775809), intmin('int64'));
asserts.isequal(uint64(-5), uint64(0));
asserts.isequal(uint32(-5), uint32(0));
% A literal whose magnitude needs more than 64 bits saturates (it must not
% throw while converting the digit string).
asserts.isequal(int64(99999999999999999999999999999), intmax('int64'));
asserts.isequal(int64(-99999999999999999999999999999), intmin('int64'));
asserts.isequal(uint64(99999999999999999999999999999), intmax('uint64'));
asserts.isequal(99999999999999999999999999999i64, intmax('int64'));
%=============================================================================
% The exact-text conversion applies only to a bare integer literal. A floating
% literal (written with a '.' or an exponent), a unary plus, a variable and an
% expression keep the ordinary double-then-cast behavior.
asserts.isequal(mat2str(int64(9007199254740993.0)), '9007199254740992');
asserts.isequal(mat2str(int64(+9007199254740993)), '9007199254740992');
asserts.isequal(mat2str(int64(1e19)), mat2str(intmax('int64')));
x = 9007199254740993;
asserts.isequal(mat2str(int64(x)), '9007199254740992');
asserts.isequal(mat2str(int64(9007199254740993 + 0)), '9007199254740992');
%=============================================================================
% The exact conversion also holds inside a matrix or a cell element.
M = [int64(9007199254740993), int64(2)];
asserts.isequal(mat2str(M(1)), '9007199254740993');
C = {int64(9007199254740993)};
asserts.isequal(mat2str(C{1}), '9007199254740993');
asserts.isequal(mat2str([int64(9007199254740993)]), '9007199254740993');
%=============================================================================
% The class is unchanged.
asserts.isequal(class(int64(9007199254740993)), 'int64');
asserts.isequal(class(uint64(9007199254740993)), 'uint64');
%=============================================================================
% int64 used as a variable is still indexed, not treated as a conversion.
int64 = [10 20 30];
asserts.isequal(int64(2), 20);
%=============================================================================
