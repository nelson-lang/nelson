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
% (2a) non-double numeric arrays are converted according to their real class.
asserts.isequal(sprintf('%f ', single([1.5 2.5])), '1.500000 2.500000 ');
asserts.isequal(sprintf('%d', int32([3 4])), '34');
%=============================================================================
% (2b) %d/%i/%o/%u/%x with a non-integer or non-finite value falls back to %e.
asserts.isequal(sprintf('%d', 2.5), '2.500000e+00');
asserts.isequal(sprintf('%d', Inf), 'Inf');
asserts.isequal(sprintf('%d', NaN), 'NaN');
asserts.isequal(sprintf('%u', -1), '-1.000000e+00');
%=============================================================================
% (2c) %s with a numeric argument: integer -> character, else %e.
asserts.isequal(sprintf('%s', 65), 'A');
asserts.isequal(sprintf('%s', [72 73]), 'HI');
asserts.isequal(sprintf('%s', 3.14), '3.140000e+00');
%=============================================================================
% (2d) %s honours width and precision.
asserts.isequal(sprintf('%5s', 'ab'), '   ab');
asserts.isequal(sprintf('%5.2s', 'abcdef'), '   ab');
%=============================================================================
% (2e) %c keeps full code points; char arrays are consumed character by character.
asserts.isequal(sprintf('%c', 8364), char(8364));
asserts.isequal(sprintf('%c', 'abc'), 'abc');
asserts.isequal(sprintf('%d%d', 'ab'), '9798');
%=============================================================================
% (2f) %g of a complex value uses the real part.
asserts.isequal(sprintf('%g', 1i), '0');
%=============================================================================
% (2g) missing data stops formatting instead of raising an error.
asserts.isequal(sprintf('abc %s'), 'abc ');
asserts.isequal(sprintf('%d %d', 1), '1 ');
%=============================================================================
% (2g) \xHH and \OOO escape sequences are interpreted.
asserts.isequal(sprintf('\x41'), 'A');
asserts.isequal(sprintf('\101'), 'A');
%=============================================================================
% (2h) %d/%i accept the signed 64-bit range, %o/%u/%x/%X the unsigned one;
% any other integer value falls back to %e, keeping flags, width, precision.
asserts.isequal(sprintf('%d', intmin('int64')), '-9223372036854775808');
asserts.isequal(sprintf('%d', -2^63), '-9223372036854775808');
asserts.isequal(sprintf('%d', 1e15), '1000000000000000');
asserts.isequal(sprintf('%d', 1e16), '10000000000000000');
asserts.isequal(sprintf('%d', 2^63), '9.223372e+18');
asserts.isequal(sprintf('%i', 2^63), '9.223372e+18');
asserts.isequal(sprintf('%d', 2^64), '1.844674e+19');
asserts.isequal(sprintf('%d', 1e20), '1.000000e+20');
asserts.isequal(sprintf('%d', -1e20), '-1.000000e+20');
asserts.isequal(sprintf('%d', 1e300), '1.000000e+300');
asserts.isequal(sprintf('%d', intmax('uint64')), '1.844674e+19');
asserts.isequal(sprintf('%d', uint64(2)^63), '9.223372e+18');
asserts.isequal(sprintf('%u', 2^63), '9223372036854775808');
asserts.isequal(sprintf('%u', 1e20), '1.000000e+20');
asserts.isequal(sprintf('%u', intmin('int64')), '-9.223372e+18');
asserts.isequal(sprintf('%x', 2^63), '8000000000000000');
asserts.isequal(sprintf('%X', 2^63), '8000000000000000');
asserts.isequal(sprintf('%x', 2^64 - 2048), 'fffffffffffff800');
asserts.isequal(sprintf('%x', intmax('uint64')), 'ffffffffffffffff');
asserts.isequal(sprintf('%x', 2^64), '1.844674e+19');
asserts.isequal(sprintf('%x', 1e20), '1.000000e+20');
asserts.isequal(sprintf('%x', int64(-1)), '-1.000000e+00');
asserts.isequal(sprintf('%u', int8(-1)), '-1.000000e+00');
asserts.isequal(sprintf('%o', 2^63), '1000000000000000000000');
asserts.isequal(sprintf('%5d|', 1e20), '1.000000e+20|');
asserts.isequal(sprintf('%-25d|', 1e20), '1.000000e+20             |');
asserts.isequal(sprintf('%025d|', 1e20), '00000000000001.000000e+20|');
asserts.isequal(sprintf('%+d|', 1e20), '+1.000000e+20|');
asserts.isequal(sprintf('%.3d|', 1e20), '1.000e+20|');
asserts.isequal(sprintf('%d', single(16777217)), '16777216');
%=============================================================================
% (2i) a single 'l' or 'h' length modifier is accepted and ignored.
asserts.isequal(sprintf('%ld', 5), '5');
asserts.isequal(sprintf('%hd', 5), '5');
asserts.isequal(sprintf('%lu', 5), '5');
asserts.isequal(sprintf('%5ld|', 8), '    8|');
asserts.isequal(sprintf('%-5ld|', 8), '8    |');
asserts.isequal(sprintf('%.3ld|', 8), '008|');
asserts.isequal(sprintf('%05ld|', 8), '00008|');
asserts.isequal(sprintf('%lx|', 255), 'ff|');
asserts.isequal(sprintf('%hx|', 255), 'ff|');
asserts.isequal(sprintf('%lo|', 8), '10|');
asserts.isequal(sprintf('%li|', 8), '8|');
asserts.isequal(sprintf('%hu|', 1), '1|');
%=============================================================================
% (2j) %c and %s with a value that is not a character code use %e;
% %s consumes a run of character codes and stops at the first other value.
asserts.isequal(sprintf('%c', 65.5), '6.550000e+01');
asserts.isequal(sprintf('%c', -1), '-1.000000e+00');
asserts.isequal(sprintf('%c', 1e20), '1.000000e+20');
asserts.isequal(sprintf('%c', 65536), '6.553600e+04');
asserts.isequal(sprintf('%c', 1114111), '1.114111e+06');
asserts.isequal(sprintf('%c', NaN), 'NaN');
asserts.isequal(sprintf('%c', Inf), 'Inf');
asserts.isequal(sprintf('%5c|', 65), '    A|');
asserts.isequal(sprintf('%-5c|', 65), 'A    |');
asserts.isequal(sprintf('%c', 55357), char(55357));
asserts.isequal(sprintf('%s', 65.5), '6.550000e+01');
asserts.isequal(sprintf('%s', 1e20), '1.000000e+20');
asserts.isequal(sprintf('%s', -1), '-1.000000e+00');
asserts.isequal(sprintf('%s', intmax('uint64')), '1.844674e+19');
asserts.isequal(sprintf('%s|', [65.5 66]), '6.550000e+01|B|');
asserts.isequal(sprintf('%s|', [65 66.5]), 'A|6.650000e+01|');
%=============================================================================
% (2k) NaN never carries a sign, an escaped \0 is a character, an empty
% format is accepted and a format without conversion is written once.
asserts.isequal(sprintf('%+f|', NaN), 'NaN|');
asserts.isequal(sprintf('%+f|', Inf), '+Inf|');
asserts.isequal(sprintf('%+f|', -Inf), '-Inf|');
asserts.isequal(sprintf('\0'), char(0));
asserts.isequal(sprintf('a\0b'), ['a', char(0), 'b']);
asserts.isequal(size(sprintf('')), [1 0]);
asserts.isequal(sprintf('abc', 5), 'abc');
%=============================================================================
