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
assert_isequal(nargin('diag'), 2);
assert_isequal(nargout('diag'), 1);
%=============================================================================
v = [20 10 -10 -20 -50];
R = diag(v);
REF = [20     0     0     0     0;
       0     10     0     0     0;
       0     0    -10     0     0;
       0     0     0    -20     0;
       0     0     0     0    -50];
assert_isequal(R, REF);
%=============================================================================
R = diag(zeros(3, 0));
REF = zeros(0, 1);
assert_isequal(R, REF);
%=============================================================================
R = diag(char(zeros(3, 0)));
REF = char(zeros(0, 1));
assert_isequal(R, REF);
%=============================================================================
v = [20 10 -10 -20 -50];
R = diag(v, 2);
REF = [     0     0    20     0     0     0     0;
     0     0     0    10     0     0     0;
     0     0     0     0   -10     0     0;
     0     0     0     0     0   -20     0;
     0     0     0     0     0     0   -50;
     0     0     0     0     0     0     0;
     0     0     0     0     0     0     0];
assert_isequal(R, REF);
%=============================================================================
v = [20 10 -10 -20 -50];
R = diag(v, -2);
REF = [     0     0     0     0     0     0     0;
     0     0     0     0     0     0     0;
    20     0     0     0     0     0     0;
     0    10     0     0     0     0     0;
     0     0   -10     0     0     0     0;
     0     0     0   -20     0     0     0;
     0     0     0     0   -50     0     0];
assert_isequal(R, REF);
%=============================================================================
M = [1 2 3 4 5;
     6 7 8 9 10;
     11 12 13 14 15];
R = diag(M);
REF = [1; 7; 13];
assert_isequal(R, REF);
%=============================================================================
R = diag(M, 1);
REF = [2; 8; 14];
assert_isequal(R, REF);
%=============================================================================
M = [1 2 3 4 5;
     6 7 8 9 10;
     11 12 13 14 15];
R = diag(diag(M));
REF = [     1     0     0;
     0     7     0;
     0     0    13];
assert_isequal(R, REF);
%=============================================================================
R = diag([]);
REF = [];
assert_isequal(R, REF);
%=============================================================================
