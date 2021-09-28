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
x = -1:0.4:1;
y = -1:0.4:1;
[X, Y] = meshgrid(x, y);
REF_X = [-1.0000   -0.6000   -0.2000    0.2000    0.6000    1.0000;
   -1.0000   -0.6000   -0.2000    0.2000    0.6000    1.0000;
   -1.0000   -0.6000   -0.2000    0.2000    0.6000    1.0000;
   -1.0000   -0.6000   -0.2000    0.2000    0.6000    1.0000;
   -1.0000   -0.6000   -0.2000    0.2000    0.6000    1.0000;
   -1.0000   -0.6000   -0.2000    0.2000    0.6000    1.0000];
assert_isapprox(X, REF_X, 1e-4);
%=============================================================================
REF_Y = [-1.0000   -1.0000   -1.0000   -1.0000   -1.0000   -1.0000;
   -0.6000   -0.6000   -0.6000   -0.6000   -0.6000   -0.6000;
   -0.2000   -0.2000   -0.2000   -0.2000   -0.2000   -0.2000;
    0.2000    0.2000    0.2000    0.2000    0.2000    0.2000;
    0.6000    0.6000    0.6000    0.6000    0.6000    0.6000;
    1.0000    1.0000    1.0000    1.0000    1.0000    1.0000];
assert_isapprox(Y, REF_Y, 1e-4);
%=============================================================================
[X, Y, Z] = meshgrid([], [], []);
assert_isequal(X, [])
assert_isequal(Y, [])
assert_isequal(Z, [])
%=============================================================================
x = 0:2:4;
y = 0:1:4;
z = 0:3:4;
[X,Y,Z] = meshgrid(x, y, z);

X_1 = [0     2     4;
     0     2     4;
     0     2     4;
     0     2     4;
     0     2     4];
assert_isequal(X(:,:,1), X_1)
%=============================================================================
X_2 = [0     2     4;
     0     2     4;
     0     2     4;
     0     2     4;
     0     2     4];
assert_isequal(X(:,:,2), X_2)
%=============================================================================
Y_1 = [0     0     0;
     1     1     1;
     2     2     2;
     3     3     3;
     4     4     4];
assert_isequal(Y(:,:,1), Y_1)
%=============================================================================
Y_2 = [0     0     0;
     1     1     1;
     2     2     2;
     3     3     3;
     4     4     4];
assert_isequal(Y(:,:,2), Y_2)
%=============================================================================
Z_1 = [0     0     0;
0     0     0;
0     0     0;
0     0     0;
0     0     0];
assert_isequal(Z(:,:,1), Z_1);
%=============================================================================
