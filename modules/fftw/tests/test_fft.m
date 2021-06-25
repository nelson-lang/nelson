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
assert_isequal(nargin('fft'), 3);
assert_isequal(nargout('fft'), 1);
%=============================================================================
x = [1 2 1];
y = fft(x);
REF = [4.0000+0.0000i, -0.5000-0.8660i, -0.5000+0.8660i];
assert_isapprox(y,REF,  1e-4);
%=============================================================================
N = 640;
n = 4;
t = 2*pi*(0:1:N-1) * inv(N);
s = cos (n * t);
S = fft(cos(n * t));
REF = zeros(size(t, 1), size(t, 2));
REF(n + 1) = N * inv(2);
REF(N - n + 1) = N * inv(2);
assert_isapprox(real(S), real(REF), 4*N*eps());
%=============================================================================
n = [0:29];
x = cos(2 * pi * n * inv(10));
N1 = 8;
X = fft(x, N1);
REF = [-1.1180+0.0000i, 3.0882-2.3972i, 0.6910-0.4271i, 0.5298-0.1611i, 0.5000+0.0000i, 0.5298+0.1611i, 0.6910+0.4271i, 3.0882+2.3972i];
assert_isapprox(X, REF, 1e-4)
%=============================================================================
X = fft(eye(2), [], 1);
REF = [1, 1; 1, -1];
assert_isequal(X, REF);
%=============================================================================
X = fft(eye(2), 2, 1);
REF = [1, 1; 1, -1];
assert_isequal(X, REF);
%=============================================================================
X = fft(eye(2, 3), [], 1);
REF = [1, 1, 0; 1, -1, 0];
assert_isequal(X, REF);
%=============================================================================
X = fft(eye(2), [], 2);
REF = [1, 1; 1, -1];
assert_isapprox(X, REF, 1e-5);
%=============================================================================
X = fft(3);
assert_isequal(X, 3);
%=============================================================================
fft(3, [], 1)
assert_isequal(X, 3);
%=============================================================================
fft(3, [], 3)
assert_isequal(X, 3);
%=============================================================================
assert_isequal(fft([]), []);
%=============================================================================
Y = fft(ones(3,3), 3, 1);
REF = [3 3 3;0 0 0;0 0 0];
assert_isequal(Y, REF);
%=============================================================================
Y = fft(ones(3,2), 3, 1);
REF = [3, 3; 0, 0; 0, 0];
assert_isequal(Y, REF);
%=============================================================================
