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
A = [3 2 4; -1 1 2; 9 5 10];
R = rank(A);
REF = 2;
assert_isequal(R, REF);
%=============================================================================
R = rank ([]);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
A = [1 2 3 4 5 6 7;
      4 5 6 7 8 9 12;
      1 2 3 4 5 6 7;
      4 5 6 7 8 9 12;
      3 4 5 6 7 8 9;
      4 5 6 7 8 9 10;
      5 6 7 8 9 10 11];
R = rank(A);
REF = 3;
assert_isequal(R, REF);
%=============================================================================
A = eye (100);
R = rank(A);
REF = 100;
assert_isequal(R, REF);
%=============================================================================
R = rank([1:9]);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
A = eye (100);
R = rank (A, 0.0009);
REF = 100;
assert_isequal(R, REF);
%=============================================================================
A = eye (100);
R = rank (A, 0.0006);
REF = 100;
assert_isequal(R, REF);
%=============================================================================
A = eye (100);
R = rank (A, 0.00000002);
REF = 100;
assert_isequal(R, REF);
%=============================================================================
