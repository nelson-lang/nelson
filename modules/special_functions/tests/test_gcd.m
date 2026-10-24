%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('gcd'), 2);
asserts.isequal(nargout('gcd'), 1);
%=============================================================================
R = gcd([], []);
REF = [];
asserts.isequal(R, REF);
%=============================================================================
R = gcd(ones(3, 0), 3);
REF = ones(3, 0);
asserts.isequal(R, REF);
%=============================================================================
R = gcd(3, ones(3, 0));
REF = ones(3, 0);
asserts.isequal(R, REF);
%=============================================================================
R = gcd([6 10; 12 15], [9 12; 16 20]);
REF = [3 2; 4 5];
asserts.isequal(R, REF);
%=============================================================================
A = [-5 17; 10 0];
B = [-15 3; 100 0];
R = gcd(A, B);
REF = [5 1; 10 0];
asserts.isequal(R, REF);
%=============================================================================
R = gcd([200, 300, 50, 35], 5);
REF = [5 5 5 5];
asserts.isequal(R, REF);
%=============================================================================
R = gcd(int16([200, 300, 50, 35]), 5);
REF = int16([5 5 5 5]);
asserts.isequal(R, REF);
%=============================================================================
R = gcd(uint64([200, 300, 50, 35]), 5);
REF = uint64([5 5 5 5]);
asserts.isequal(R, REF);
%=============================================================================
R = gcd(int16([100 -30 200]), int16([20 15 9]));
REF = int16([20 15 1]);
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('gcd([18-i, -29+3i], -3-4i)', _('Inputs must be real integers.'))
% the real-integer condition also carries a stable identifier
asserts.checkerror('gcd([18-i, -29+3i], -3-4i)', _('Inputs must be real integers.'), 'Nelson:gcd:mustBeRealIntegers')
%=============================================================================
% Bezout coefficients: [G, C, D] such that C.*A + D.*B == G.
[g, c, d] = gcd(12, 18);
asserts.isequal([g c d], [6 -1 1]);
[g, c, d] = gcd(18, 12);
asserts.isequal([g c d], [6 1 -1]);
[g, c, d] = gcd(-12, 18);
asserts.isequal([g c d], [6 1 1]);
[g, c, d] = gcd(0, 0);
asserts.isequal([g c d], [0 0 0]);
[g, c, d] = gcd(0, 5);
asserts.isequal([g c d], [5 0 1]);
%=============================================================================
[g, c, d] = gcd([12 8 15], [18 12 25]);
asserts.isequal(g, [6 4 5]);
asserts.isequal(c, [-1 -1 2]);
asserts.isequal(d, [1 1 -1]);
asserts.isequal(c .* [12 8 15] + d .* [18 12 25], g);
%=============================================================================
[g, c, d] = gcd(int32(12), int32(18));
asserts.isequal([g c d], int32([6 -1 1]));
asserts.isequal(class(c), 'int32');
%=============================================================================
[g, c, d] = gcd(int8(12), int8(18));
asserts.isequal([g c d], int8([6 -1 1]));
%=============================================================================
asserts.checkerror('[g, c] = gcd(uint16(100), uint16(60))', ...
  'Unsigned integers are not supported for the three output syntax of GCD.');
%=============================================================================
asserts.checkerror('gcd', _('Not enough input arguments.'));
%=============================================================================
