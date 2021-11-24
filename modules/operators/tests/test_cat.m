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
R = cat(2, zeros(0, 1), zeros(0, 2));
REF = zeros(0, 3);
assert_isequal(R, REF);
%=============================================================================
R = cat(2, [1 2], []);
REF = [1 2]; 
assert_isequal(R, REF);
%=============================================================================
M1 = [1 2; 3 4];
M2 = [5 6; 7 8];
A1 = {M1,M2};
R = cat(1,A1{:});
REF = [ 1     2;
3     4;
5     6;
7     8];
assert_isequal(R, REF);
%=============================================================================
M1 = [1 2; 3 4];
M2 = [5 6; 7 8];
A1 = {M1,M2};
R = cat(2,A1{:});
REF = [1     2     5     6;
    3     4     7     8];
assert_isequal(R, REF);
%=============================================================================
A = rand(2, 3, 4);
B = rand(2, 3, 5);
C = cat(3, A, B);
R = size(C);
REF =   [2     3     9];
assert_isequal(R, REF);
%=============================================================================
A = ones(3);
B = zeros(3);
R = cat(1, A, B);
REF = [1     1     1;
1     1     1;
1     1     1;
0     0     0;
0     0     0;
0     0     0];
assert_isequal(R, REF);
%=============================================================================
A = ones(3);
B = zeros(3);
R = cat(2, A, B);
REF = [     1     1     1     0     0     0;
1     1     1     0     0     0;
1     1     1     0     0     0];
assert_isequal(R, REF);
%=============================================================================
A = ones(3);
B = zeros(3);
C = cat(3, A, B);
R = size(C);
REF =   [3     3     2];
assert_isequal(R, REF);
%=============================================================================
