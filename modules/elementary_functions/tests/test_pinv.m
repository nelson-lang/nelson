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
A = [1, 2, 3; 4, 5, 6];
R = pinv(A);
REF = [-0.9444      0.4444;
-0.1111      0.1111;
0.7222     -0.2222];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1, 2, 3; 4, 5, 6];
R = pinv(A, 1);
REF = [     0.0174      0.0416;
0.0230      0.0549;
0.0286      0.0683];
assert_isapprox(R, REF, 1e-3);
%=============================================================================
A = [1, 2, 3; 4, 5, 6+7i];
R = pinv(A, 2.0);
REF = [0.0075 + 0.0045i   0.0282 - 0.0011i;
0.0098 + 0.0056i   0.0365 - 0.0022i;
0.0200 - 0.0052i   0.0449 - 0.0497i];
assert_isapprox(abs(R), abs(REF), 1e-3);
%=============================================================================
