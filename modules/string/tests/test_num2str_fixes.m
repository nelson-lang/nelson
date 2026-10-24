%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% (4a) real-valued single complex uses the real part, not the interleaved data.
asserts.isequal(num2str(complex(single([1 2 3]), single(0))), '1  2  3');
%=============================================================================
% (4b) integer conversion format applied to an integer value.
asserts.isequal(num2str(5, '%d'), '5');
asserts.isequal(num2str(5, '%5d'), '5');
%=============================================================================
% (4c) default precision matches expected behavior (5 significant digits for pi).
asserts.isequal(num2str(pi), '3.1416');
asserts.isequal(num2str(single(pi)), '3.1416');
asserts.isequal(num2str(123.456), '123.456');
asserts.isequal(num2str(1234.5678), '1234.5678');
asserts.isequal(num2str(0.000123456), '0.00012346');
%=============================================================================
% Scalar complex precision is unchanged (6 significant digits).
asserts.isequal(num2str(complex(3 * pi, 2 * pi)), '9.42478+6.28319i');
%=============================================================================
% num2str(x, format): the format is applied with sprintf semantics to chunks of
% values (one per conversion), chunks are right-aligned and common blank columns removed;
% the imaginary part uses the format with a forced sign, no width, followed by i.
asserts.isequal(num2str([1 2 3], '%d'), '123');
asserts.isequal(num2str([1 2 3], '%d '), '1 2 3');
asserts.isequal(num2str([1 2; 3 4], '%d '), ['1 2'; '3 4']);
asserts.isequal(num2str([1 22; 333 4], '%d'), ['  1 22'; '333  4']);
asserts.isequal(num2str([1 22; 333 4], '%-5d'), ['1    22'; '333  4 ']);
asserts.isequal(num2str([1; 22], '%d'), [' 1'; '22']);
asserts.isequal(num2str([1 2 3; 40 5 6], '%d'), [' 1 2 3'; '40 5 6']);
asserts.isequal(num2str([1 20 3], '%d'), '120 3');
asserts.isequal(num2str(-[1 20 3], '%d'), '-1-20 -3');
asserts.isequal(num2str([1 2 3], '%d %d'), '1 2  3');
asserts.isequal(num2str([1 2 3], '%d,%d'), '1,2 3,');
asserts.isequal(num2str(1:5, '[%d %d]'), '[1 2][3 4]   [5');
asserts.isequal(num2str([1 2 3], 'a%db%dc'), 'a1b2c  a3b');
asserts.isequal(num2str([1.5 2; 3 4.25], '%g'), ['1.5   2'; '  34.25']);
asserts.isequal(num2str([1 2; 3 4], '%d, '), ['1, 2,'; '3, 4,']);
asserts.isequal(num2str([1 2 3], '%d\n'), ['1', char(10), '2', char(10), '3']);
asserts.isequal(num2str([1 2], '\t%d'), ['1', char(9), '2']);
asserts.isequal(num2str([1 2; 3 4; 50 6], 'a%db'), [' a1b a2b'; ' a3b a4b'; 'a50b a6b']);
asserts.isequal(num2str([1 2 3], '%%'), '%');
asserts.isequal(num2str(pi, '%d'), '3.141593e+00');
asserts.isequal(num2str(2.5, '%d'), '2.500000e+00');
asserts.isequal(num2str(1e20, '%d'), '1.000000e+20');
asserts.isequal(num2str(NaN, '%d'), 'NaN');
asserts.isequal(num2str(Inf, '%d'), 'Inf');
asserts.isequal(num2str(int8([1 2 3]), '%d'), '123');
asserts.isequal(num2str(uint8([1 2 3]), '%d'), '123');
asserts.isequal(num2str(int64(9223372036854775807), '%d'), '9223372036854775807');
asserts.isequal(num2str(uint64(18446744073709551615), '%d'), '1.844674e+19');
asserts.isequal(num2str([65 66 67], '%s'), 'ABC');
asserts.isequal(num2str(65, '%c'), 'A');
asserts.isequal(num2str(65.5, '%c'), '6.550000e+01');
asserts.isequal(num2str(3.5, '%s'), '3.500000e+00');
asserts.isequal(num2str([65 66 67], '%44'), '');
asserts.isequal(num2str([1 2 3], '%'), '');
asserts.isequal(num2str('abc', '%d'), 'abc');
asserts.isequal(num2str(true, '%d'), '1');
asserts.isequal(num2str([true false], '%d'), '10');
asserts.isequal(num2str(sparse([1 0 2]), '%d'), '102');
asserts.isequal(num2str([], '%d'), '');
asserts.isequal(num2str(1+2i, '%d'), '1+2i');
asserts.isequal(num2str(1+2i, '%5.1f'), '1.0+2.0i');
asserts.isequal(num2str(-1-2i, '%5.1f'), '-1.0-2.0i');
asserts.isequal(num2str(1+2i, '%-5.1f|'), '1.0  |+2.0i|');
asserts.isequal(num2str(1+2i, '  %5.1f'), '1.0  +2.0i');
asserts.isequal(num2str(1+2i, '%-5.1f'), '1.0+2.0i');
asserts.isequal(num2str(complex(1.5, -2.25), '%g'), '1.5-2.25i');
asserts.isequal(num2str(complex(1.5, -2.25), '%+g'), '+1.5-2.25i');
asserts.isequal(num2str(complex(1.5, -2.25), '%08.3f'), '0001.500-2.250i');
asserts.isequal(num2str(complex(1.5, -2.25), '%#g'), '1.50000-2.25000i');
asserts.isequal(num2str(complex(1.5, -2.25), '%e'), '1.500000e+00-2.250000e+00i');
asserts.isequal(num2str(complex(NaN, NaN), '%5.1f'), 'NaN+NaNi');
asserts.isequal(num2str(complex(Inf, -Inf), '%5.1f'), 'Inf-Infi');
asserts.isequal(num2str(complex(65, 66), '%c'), 'ABi');
asserts.isequal(num2str([1+2i 3-4i], '%d'), '1+2i3-4i');
asserts.isequal(num2str([1+2i 3-4i], '%5.1f'), '1.0+2.0i  3.0-4.0i');
asserts.isequal(num2str([1+2i; 3-4i], '%5.1f'), ['1.0+2.0i'; '3.0-4.0i']);
asserts.isequal(num2str(complex(1, 2) * [1 2], '%d%d'), '12+2i+4i');
asserts.isequal(num2str(complex(1, 2) * [1 2], '%d,%d'), '1,2+2i,+4i');
asserts.isequal(num2str(complex(1, 2) * [1 2; 3 40], '%d'), ['1+2i 2+4i '; '3+6i40+80i']);
asserts.isequal(num2str(complex(1, 2), 'a%db'), 'a1ba+2ib');
asserts.isequal(num2str(eye(3) + 1i, '%d'), ['1+1i0+1i0+1i'; '0+1i1+1i0+1i'; '0+1i0+1i1+1i']);
asserts.isequal(num2str(eye(3) + 1i, '%5.2f'), ['1.00+1.00i 0.00+1.00i 0.00+1.00i'; '0.00+1.00i 1.00+1.00i 0.00+1.00i'; '0.00+1.00i 0.00+1.00i 1.00+1.00i']);
asserts.isequal(num2str(complex(1, 0), '%5.1f'), '1.0');
%=============================================================================
% a string scalar is returned as characters, a missing string as an empty text.
asserts.isequal(num2str("abc"), 'abc');
asserts.isequal(num2str(string(missing)), '');
asserts.isequal(num2str("abc", '%d'), 'abc');
%=============================================================================
