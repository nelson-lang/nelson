%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
str = '2.7183  3.1416  0.0073';
R = sscanf(str, '%f');
REF = [2.7183; 3.1416; 0.0073];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
str = "2.7183  3.1416  0.0073";
R = sscanf(str, '%f', [1 3]);
REF = [2.7183    3.1416    0.0073];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
str = "2.7183  3.1416  0.0073";
R = sscanf(str,'%f',[2 2]);
REF = [ 2.7183    0.0073; 3.1416         0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
R = sscanf(T, '%d°F');
REF = [78; 72; 64; 66; 49];
assert_isequal(R, REF);
%=============================================================================
str = '3.14159 are the first 6 digits of pi';
[R, n, errmsg] = sscanf(str,'%f');
REF = 3.1416;
n_REF = 1;
errmsg_REF = _('Matching failure in format.');
assert_isapprox(R, REF, 1e-4);
assert_isequal(n, n_REF);
assert_isequal(errmsg, errmsg_REF);
%=============================================================================
str = '3.14159 are the first 6 digits of pi';
[A, n, errmsg, nextindex] = sscanf(str,'%f');
assert_isequal(str(nextindex:end), 'are the first 6 digits of pi')
%=============================================================================
str = '⛔3.1416⛔';
[A, n, errmsg, i] = sscanf(str,'%s%f%s');
REF = [9940
51
46
49
52
49
54
9940];
assert_isequal(A, REF);
assert_isequal(n, 1);
assert_isequal(errmsg, '');
assert_isequal(i, 9);
%=============================================================================
str = 'A   A';
R = sscanf(str, '%s');
assert_isequal(R, 'AA');
%=============================================================================
str = 'A   A';
R = sscanf(str, '%s%d');
assert_isequal(R, 65);
%=============================================================================
str = '2.7183  3.1416  0.0073';
[A, n, errmsg, nextindex] = sscanf(str,'%s%k');
assert_isequal(A,  '2.7183');
assert_isequal(n, 1)
assert_isequal(errmsg, _('Invalid format.'))
assert_isequal(nextindex, 7)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('', '%s%d');
assert_isequal(A, []);
assert_isequal(n, 0)
assert_isequal(errmsg, '')
assert_isequal(nextindex, 1)
%=============================================================================
str = '2.7183  3.1416  0.0073';
[A, n, errmsg, nextindex] = sscanf(str,'%ls');
assert_isequal(A,  '');
assert_isequal(n, 0)
assert_isequal(errmsg, _('Invalid format.'))
assert_isequal(nextindex, 1)
%=============================================================================
str = 'говорити hovoryty 漢字';
[a, b, c, d] =sscanf(str, '%s %s %s');
assert_isequal(a, 'говоритиhovoryty漢字')
assert_isequal(b, 3)
assert_isequal(c, '')
assert_isequal(d, 21)
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
[A, n, errmsg, nextindex] = sscanf(T, '%d°F');
assert_isequal(A, [78; 72; 64; 66;49])
assert_isequal(n, 5)
assert_isequal(errmsg, '')
assert_isequal(nextindex, 25)
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
[A, n, errmsg, nextindex] = sscanf(T, '%ld°F');
assert_isequal(A, int64([78; 72; 64; 66;49]))
assert_isequal(n, 5)
assert_isequal(errmsg, '')
assert_isequal(nextindex, 25)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('1.3e4', '%lf');
assert_isequal(A, 13000)
assert_isequal(n, 1)
assert_isequal(errmsg, '')
assert_isequal(nextindex, 6)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('1.3d4', '%lf');
assert_isapprox(A, 1.3, 1e-4)
assert_isequal(n, 1)
assert_isequal(errmsg, _('Matching failure in format.'))
assert_isequal(nextindex, 4)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('1.3d4', '%f');
assert_isapprox(A, 1.3, 1e-4)
assert_isequal(n, 1)
assert_isequal(errmsg, _('Matching failure in format.'))
assert_isequal(nextindex, 4)
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
[A, n, errmsg, nextindex] = sscanf(T, '%lx°F');
assert_isequal(A, uint64([120; 114; 100; 102; 73]))
assert_isequal(n, 5)
assert_isequal(errmsg, '')
assert_isequal(nextindex, 25)
%=============================================================================
