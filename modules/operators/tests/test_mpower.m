%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
A = [1 2; 3 4];
R = A^2;
REF = [ 7    10; 15    22];
assert_isequal(R, REF);
%=============================================================================
B = [0 1; 1 0];
R = 2^B;
REF = [   1.2500    0.7500; 0.7500    1.2500];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
B = [0 1; 1 0];
[V, D] = eig(B);
R = V*2^(D/V);
REF = [-0.475060937282411   1.561877475526996;
 1.561877475526996   0.475060937282411];
 assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1 1; 0 1];
R = A^(0.5);
REF = [ 1     0;0     1];
assert_isequal(R, REF); 
%=============================================================================
R = 4^eye(3,3);
REF = eye(3, 3) * 4;
assert_isequal(R, REF) 
%=============================================================================