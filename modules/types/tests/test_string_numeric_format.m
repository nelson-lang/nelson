%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% integer values print every digit below 1e16, larger magnitudes use 16
% significant digits
asserts.isequal(string(0), "0");
asserts.isequal(string(-0), "0");
asserts.isequal(string(100), "100");
asserts.isequal(string(123456789), "123456789");
asserts.isequal(string(1e15), "1000000000000000");
asserts.isequal(string(2^53), "9007199254740992");
asserts.isequal(string(2^53 + 2), "9007199254740994");
asserts.isequal(string(1e16), "1e+16");
asserts.isequal(string(1e17), "1e+17");
asserts.isequal(string(1.2345678901234567e16), "1.234567890123457e+16");
asserts.isequal(string(-2^60), "-1.152921504606847e+18");
asserts.isequal(string(1e300), "1e+300");
%=============================================================================
% fractions keep 5 significant digits, trailing zeros are removed
asserts.isequal(string(1.5), "1.5");
asserts.isequal(string(0.1), "0.1");
asserts.isequal(string(1 / 3), "0.33333");
asserts.isequal(string(pi), "3.1416");
asserts.isequal(string(-pi), "-3.1416");
asserts.isequal(string(123.456), "123.456");
asserts.isequal(string(12345.6789), "12345.6789");
asserts.isequal(string(100000.5), "100000.5");
asserts.isequal(string(12345678.9), "12345678.9");
asserts.isequal(string(99999.99999), "100000");
asserts.isequal(string(0.999999), "1");
%=============================================================================
% small magnitudes
asserts.isequal(string(1.5e-4), "0.00015");
asserts.isequal(string(0.00012345), "0.00012345");
asserts.isequal(string(1e-4), "0.0001");
asserts.isequal(string(1e-5), "1e-05");
asserts.isequal(string(1.5e-5), "1.5e-05");
asserts.isequal(string(1e-300), "1e-300");
%=============================================================================
% special values and other classes
asserts.isequal(string(Inf), "Inf");
asserts.isequal(string(-Inf), "-Inf");
asserts.isequal(string(single(1 / 3)), "0.33333");
asserts.isequal(string(single(1e10)), "10000000000");
asserts.isequal(string(int8(5)), "5");
asserts.isequal(string(uint64(18446744073709551615)), "18446744073709551615");
asserts.isequal(string(true), "true");
%=============================================================================
% complex values use the same rules for both parts
asserts.isequal(string(1 + 2i), "1+2i");
asserts.isequal(string(0.5 - 1.25i), "0.5-1.25i");
asserts.isequal(string(1e10 + 1e-3i), "10000000000+0.001i");
asserts.isequal(string(1e16 + 1e16i), "1e+16+1e+16i");
asserts.isequal(string(-1e-5 - 1e-6i), "-1e-05-1e-06i");
%=============================================================================
% concatenation with a string uses the same conversion
asserts.isequal("x" + 1e16, "x1e+16");
asserts.isequal("x" + pi, "x3.1416");
asserts.isequal("x" + 1e-5, "x1e-05");
%=============================================================================
