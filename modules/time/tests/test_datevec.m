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
assert_isequal(nargin('datevec'), 1);
assert_isequal(nargout('datevec'), 6);
%=============================================================================
[Y, M, D] = datevec ([1 2; 3 4]);
REF = [1 2; 3 4];
assert_isequal(D, REF);
%=============================================================================
[Y, M] = datevec(ones(3, 0));
assert_isequal(size(Y), [3 0]);
%=============================================================================
[Y, M] = datevec(ones(0, 3));
assert_isequal(size(Y), [0 3]);
%=============================================================================
[Y, M] = datevec(ones(3, 3, 0));
assert_isequal(size(Y), [3 0]);
%=============================================================================
[Y, M] = datevec(ones(0, 3, 4));
assert_isequal(size(Y), [0 12]);
%=============================================================================
R = datevec([721842.33]);
assert_isequal(R, [1976, 5, 2, 7, 55, 12]);
%=============================================================================
R = datevec(ones(2, 1) * [721842.33]);
assert_isequal(R, [1976, 5, 2, 7, 55, 12; 1976, 5, 2, 7, 55, 12]);
%=============================================================================
R = datevec([721222.33, 761532.44]);
REF = [1974, 8, 21, 7, 55, 12; 2084, 12, 31, 10, 33, 36];
assert_isapprox(R, REF, 1e-1);
%=============================================================================
R = datevec([721222.33] * ones(3, 4));
REF = [1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000;
1974.0000, 8.0000, 21.0000, 7.0000, 55.0000, 12.0000];
assert_isequal(R, REF);
%=============================================================================
R = datevec([158493 247745 637308 ; 567032 499035 514298 ; 165 471293 658662]);
REF = [ 433 12 8 0 0 0;
1552 6  24  0 0 0;
0, 6  13  0 0 0;
678  4  20  0 0 0;
1366 4  23  0 0 0;
1290 5  9   0 0 0;
1744 11 20  0 0 0;
1408 2  6   0 0 0;
1803 5  10  0 0 0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(Inf);
REF = [NaN   NaN   NaN   NaN   NaN   NaN];
assert_isequal(R, REF);
%=============================================================================
R = datevec(sparse(eye(3, 3)));
REF = [     0     1     1     0     0     0;
0     1     1     0     0     0;
0     1     1     0     0     0];
assert_isequal(R, REF);
%=============================================================================
R = datevec(sparse([721222.33, 761532.44]));
REF = [1974, 8, 21, 7, 55, 12; 2084, 12, 31, 10, 33, 36];
assert_isapprox(R, REF, 1e-1);
%=============================================================================
[Y, M] = datevec(sparse(eye(3, 3)));
Y_REF = [0 0 0];
M_REF = [1 1 1];
assert_isequal(Y, Y_REF);
assert_isequal(M, M_REF);
%=============================================================================
[Y, M] = datevec(sparse([721222.33, 761532.44]));
Y_REF = [1974 2084];
M_REF = [8 12];
assert_isequal(Y, Y_REF);
assert_isequal(M, M_REF);
%=============================================================================
