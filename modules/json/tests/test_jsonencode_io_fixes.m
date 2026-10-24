%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Bug 3: large 64-bit integers used to overflow a 21-wchar buffer and crash.
asserts.isequal(jsonencode(intmax('uint64')), '18446744073709551615');
%=============================================================================
asserts.isequal(jsonencode(intmin('int64')), '-9223372036854775808');
%=============================================================================
asserts.isequal(jsonencode(intmax('int64')), '9223372036854775807');
%=============================================================================
asserts.isequal(jsonencode(intmax('uint64') - 1), '18446744073709551614');
%=============================================================================
% Bug 4: 64-bit integers must be formatted directly (no lossy double detour).
% (int64(9007199254740992) is exactly representable; +1 stays exact.)
big = int64(9007199254740992) + int64(1);
asserts.isequal(jsonencode(big), '9007199254740993');
%=============================================================================
% Bug 6: control characters below 0x20 without a short escape must be escaped
% as \uXXXX (previously chars 1..7 and 11 were emitted raw).
% Reference strings are built from explicit character codes to avoid ambiguity.
% char([65 1 66]) -> "AB"
ref = char([34 65 92 117 48 48 48 49 66 34]);
asserts.isequal(jsonencode(char([65 1 66])), ref);
%=============================================================================
% char([1 8 12 31]) -> "\b\f"
ref = char([34 92 117 48 48 48 49 92 98 92 102 92 117 48 48 49 102 34]);
asserts.isequal(jsonencode(char([1 8 12 31])), ref);
%=============================================================================
% Short escapes must still be used where they exist: "\t\n\r"
ref = char([34 92 116 92 110 92 114 34]);
asserts.isequal(jsonencode(char([9 10 13])), ref);
%=============================================================================
