%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('schur'), 2);
assert_isequal(nargout('schur'), 2);
%=============================================================================
assert_isequal(schur([]), []);
[U, T] = schur([]);
assert_isequal(U, []);
assert_isequal(T, []);
%=============================================================================
assert_checkerror('schur(NaN)', _('Input argument must not contain NaN or Inf.'));
assert_checkerror('schur(Inf)', _('Input argument must not contain NaN or Inf.'));
%=============================================================================
assert_checkerror('schur(zeros(3,5))', _('Square matrix expected.'));
%=============================================================================
A = [0.          3.          0.          0.          inv(3);
inv(3)      0.          3.          0.          0;
0.          inv(3)   0.          3.          0;
0.          0.          inv(3)   0.          3;
3.          0.          0.          inv(3)   0];
T = schur(A);
T_REF = [-2.6967    1.5674   -0.0000    0.0000    0.0000;
-1.5674   -2.6967    0.0000    0.0000    0.0000;
0         0    3.3333    0.0000    0.0000;
0         0         0    1.0301    2.5362;
0         0         0   -2.5362    1.0301];
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
T = schur(A, 'real');
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
T = schur(A, 'complex');
T_REF = [ -2.6967 + 1.5674i  -0.0000 + 0.0000i  -0.0000 + 0.0000i   0.0000 - 0.0000i  -0.0000 - 0.0000i;
0.0000 + 0.0000i  -2.6967 - 1.5674i  -0.0000 + 0.0000i  -0.0000 + 0.0000i   0.0000 + 0.0000i;
0.0000 + 0.0000i   0.0000 + 0.0000i   1.0301 + 2.5362i  -0.0000 + 0.0000i   0.0000 - 0.0000i;
0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   3.3333 + 0.0000i   0.0000 + 0.0000i;
0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   1.0301 - 2.5362i];
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
[U, T] = schur(A);
T_REF = [-2.6967233   1.5674273  -4.937D-16   8.858D-16   7.753D-16;
-1.5674273  -2.6967233   4.576D-16   8.326D-16   0.;
0.          0.          3.3333333   1.758D-15   3.138D-16;
0.          0.          0.          1.0300566   2.5361507;
0.          0.          0.         -2.5361507   1.0300566];
assert_isapprox(U * T * inv(U), A, 1e-4);
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
[U, T] = schur(A, 'real');
assert_isapprox(U * T * inv(U), A, 1e-4);
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
[U, T] = schur(A, 'complex')
T_REF = [-2.6967+1.5674i     0.0000-0.0000i    -0.0000+0.0000i     0.0000-0.0000i     0.0000-0.0000i;
0.0000    -2.6967-1.5674i     0.0000-0.0000i     0.0000+0.0000i    -0.0000+0.0000i;
0.0000     0.0000     1.0301+2.5362i     0.0000+0.0000i    -0.0000-0.0000i;
0.0000     0.0000     0.0000     3.3333+0.0000i     0.0000+0.0000i;
0.0000     0.0000     0.0000     0.0000     1.0301-2.5362i]
assert_isapprox(real(U * T * inv(U)), real(A), 1e-4);
assert_isapprox(real(T), real(T_REF), 1e-4);
%=============================================================================
A = [0.            3+i         0.            0.            0.3-0.1i;
0.3-0.1i     0.            3+i         0.            0;
0.            0.3-0.1i     0.            3+i         0;
0.            0.            0.3-0.1i     0.            3+i;
3+i         0.            0.            0.3-0.1i     0];
T = schur(A);
T_REF = [3.3000+0.9000i  -0.0000-0.0000i   0.0000-0.0000i   0.0000+0.0000i   0.0000+0.0000i;
0.0000+0.0000i   2.0659-2.2897i  -0.0000+0.0000i  -0.0000-0.0000i   0.0000-0.0000i;
0.0000+0.0000i   0.0000+0.0000i  -0.0264+2.8460i   0.0000-0.0000i  -0.0000-0.0000i;
0.0000+0.0000i   0.0000+0.0000i   0.0000+0.0000i  -3.3163+0.8589i  -0.0000-0.0000i;
0.0000+0.0000i   0.0000+0.0000i   0.0000+0.0000i   0.0000+0.0000i  -2.0232-2.3151i];
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
T = schur(A, 'real');
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
T = schur(A, 'complex')
[U, T] = schur(A)
assert_isapprox(U * T * inv(U), A, 1e-4);
%=============================================================================
[U, T] = schur(A, 'real')
assert_isapprox(T, T_REF, 1e-4);
assert_isapprox(U * T * inv(U), A, 1e-4);
%=============================================================================
[U, T] = schur(A, 'complex')
assert_isapprox(U * T * inv(U), A, 1e-4);
%=============================================================================
A = [i, 20-i, 10+i; 90, 10, 20+i; 20, 50i, 10-10i];
[U, T] = schur(A);
assert_isapprox(U * T * inv(U), A, 1e-4);
%=============================================================================
[U, T] = schur([10,40,20;20,-50,-10;-12,24,80]);
U_REF = [ 0.4542   -0.8806   -0.1350;
-0.8703   -0.4710    0.1441;
0.1905   -0.0520    0.9803];
T_REF = [ -58.2497  -19.5852   43.9714;
0   22.4746  -16.2152;
0         0   75.7751];
assert_isapprox(U, U_REF, 1e-4);
assert_isapprox(T, T_REF, 1e-4);
%=============================================================================
[U, T] = schur(-3);
assert_isequal(U, 1);
assert_isequal(T, -3);
%=============================================================================
[U, T] = schur(-3i);
assert_isequal(U, 1);
assert_isequal(T, -3i);
%=============================================================================
