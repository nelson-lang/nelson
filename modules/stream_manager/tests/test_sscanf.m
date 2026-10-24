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
str = '2.7183  3.1416  0.0073';
R = sscanf(str, '%f');
REF = [2.7183; 3.1416; 0.0073];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
str = "2.7183  3.1416  0.0073";
R = sscanf(str, '%f', [1 3]);
REF = [2.7183 3.1416 0.0073];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
str = "2.7183  3.1416  0.0073";
R = sscanf(str, '%f', [2 2]);
REF = [2.7183 0.0073; 3.1416 0];
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
R = sscanf(T, '%d°F');
REF = [78; 72; 64; 66; 49];
asserts.isequal(R, REF);
%=============================================================================
str = '3.14159 are the first 6 digits of pi';
[R, n, errmsg] = sscanf(str, '%f');
REF = 3.1416;
n_REF = 1;
errmsg_REF = _('Matching failure in format.');
asserts.isapprox(R, REF, 1e-4);
asserts.isequal(n, n_REF);
asserts.isequal(errmsg, errmsg_REF);
%=============================================================================
str = '3.14159 are the first 6 digits of pi';
[A, n, errmsg, nextindex] = sscanf(str, '%f');
asserts.isequal(str(nextindex:end), 'are the first 6 digits of pi')
%=============================================================================
str = '⛔3.1416⛔';
[A, n, errmsg, i] = sscanf(str, '%s%f%s');
REF = [9940
  51
  46
  49
  52
  49
  54
  9940];
asserts.isequal(A, REF);
asserts.isequal(n, 1);
asserts.isequal(errmsg, '');
asserts.isequal(i, 9);
%=============================================================================
str = 'A   A';
R = sscanf(str, '%s');
asserts.isequal(R, 'AA');
%=============================================================================
str = 'A   A';
R = sscanf(str, '%s%d');
asserts.isequal(R, 65);
%=============================================================================
str = '2.7183  3.1416  0.0073';
[A, n, errmsg, nextindex] = sscanf(str, '%s%k');
asserts.isequal(A, '2.7183');
asserts.isequal(n, 1)
asserts.isequal(errmsg, _('Invalid format.'))
asserts.isequal(nextindex, 7)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('', '%s%d');
asserts.isequal(A, []);
asserts.isequal(n, 0)
asserts.isequal(errmsg, '')
asserts.isequal(nextindex, 1)
%=============================================================================
str = '2.7183  3.1416  0.0073';
[A, n, errmsg, nextindex] = sscanf(str, '%ls');
asserts.isequal(A, '');
asserts.isequal(n, 0)
asserts.isequal(errmsg, _('Invalid format.'))
asserts.isequal(nextindex, 1)
%=============================================================================
str = 'говорити hovoryty 漢字';
[a, b, c, d] = sscanf(str, '%s %s %s');
asserts.isequal(a, 'говоритиhovoryty漢字')
asserts.isequal(b, 3)
asserts.isequal(c, '')
asserts.isequal(d, 21)
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
[A, n, errmsg, nextindex] = sscanf(T, '%d°F');
asserts.isequal(A, [78; 72; 64; 66; 49])
asserts.isequal(n, 5)
asserts.isequal(errmsg, '')
asserts.isequal(nextindex, 25)
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
[A, n, errmsg, nextindex] = sscanf(T, '%ld°F');
asserts.isequal(A, int64([78; 72; 64; 66; 49]))
asserts.isequal(n, 5)
asserts.isequal(errmsg, '')
asserts.isequal(nextindex, 25)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('1.3e4', '%lf');
asserts.isequal(A, 13000)
asserts.isequal(n, 1)
asserts.isequal(errmsg, '')
asserts.isequal(nextindex, 6)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('1.3d4', '%lf');
asserts.isapprox(A, 1.3, 1e-4)
asserts.isequal(n, 1)
asserts.isequal(errmsg, _('Matching failure in format.'))
asserts.isequal(nextindex, 4)
%=============================================================================
[A, n, errmsg, nextindex] = sscanf('1.3d4', '%f');
asserts.isapprox(A, 1.3, 1e-4)
asserts.isequal(n, 1)
asserts.isequal(errmsg, _('Matching failure in format.'))
asserts.isequal(nextindex, 4)
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
[A, n, errmsg, nextindex] = sscanf(T, '%lx°F');
asserts.isequal(A, uint64([120; 114; 100; 102; 73]))
asserts.isequal(n, 5)
asserts.isequal(errmsg, '')
asserts.isequal(nextindex, 25)
%=============================================================================
