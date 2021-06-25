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
T = tril(ones(2, 0));
assert_isequal(T, ones(2, 0));
%=============================================================================
T = tril([1 2 3 4]);
assert_isequal(T, [1 0 0 0]);
%=============================================================================
T = tril([1 2 3 4], 2);
assert_isequal(T, [ 1     2    3     0]);
%=============================================================================
M = [1 2 3 4; 5 6 7 8; 9 10 11 12];
T = tril(M);
REF = [    1     0     0     0;
5     6     0     0;
9    10    11     0];
assert_isequal(T, REF);
%=============================================================================
M = [1 2 3 4; 5 6 7 8; 9 10 11 12];
T = tril(M, 0);
REF = [    1     0     0     0;
5     6     0     0;
9    10    11     0];
assert_isequal(T, REF);
%=============================================================================
M = [1 2 3 4; 5 6 7 8; 9 10 11 12];
T = tril(M, 1);
REF = [     1     2     0     0;
5     6     7     0;
9    10    11    12];
assert_isequal(T, REF);
%=============================================================================
M = [1 2 3 4; 5 6 7 8; 9 10 11 12];
T = tril(M, -1);
REF = [     0     0     0     0;
5     0     0     0;
9    10     0     0];
assert_isequal(T, REF);
%=============================================================================
M = [1 2 3 4; 5 6 7 8; 9 10 11 12];
S = sparse(M);
t = tril(S);
T = full(t);
REF = [    1     0     0     0;
5     6     0     0;
9    10    11     0];
assert_isequal(T, REF);
%=============================================================================
M = [1 2 3 4; 5 6 7 8; 9 10 11 12];
N = [9 10 11 12; 5 6 7 8; 1 2 3 4];
C = complex(M, N);
T = tril(C, 1);
REF = [1.0000+9.0000i,   2.0000+10.0000i,   0.0000+0.0000i,   0.0000+0.0000i;
    5.0000+5.0000i,   6.0000+6.0000i,   7.0000+7.0000i,   0.0000+0.0000i;
    9.0000+1.0000i,  10.0000+2.0000i,  11.0000+3.0000i,  12.0000+4.0000i];
assert_isequal(T, REF);
%=============================================================================
