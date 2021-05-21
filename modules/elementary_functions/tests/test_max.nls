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
assert_isequal(nargin('max'), 4)
assert_isequal(nargout('max'), 2)
%=============================================================================
R = max([]);
assert_isequal(R, []);
%=============================================================================
[R, I] = max([]);
assert_isequal(R, []);
assert_isequal(I, []);
%=============================================================================
[R, I] = max(5);
assert_isequal(R, 5);
assert_isequal(I, 1);
%=============================================================================
[R, I] = max([1 NaN 3 Inf -Inf 4]);
assert_isequal(R, Inf);
assert_isequal(I, 4);
%=============================================================================
A = [23 42 37 18 52];
R = max(A);
assert_isequal(R, 52);
%=============================================================================
A = [23 42 37 18 52];
[R, I] = max(A);
assert_isequal(R, 52);
assert_isequal(I, 5);
%=============================================================================
A = [-2+2i, 4+i, -1-3i];
[R, I] = max(A);
assert_isequal(R, 4+i);
assert_isequal(I, 2);
%=============================================================================
A = [-2+2i, 4+i, -1-5i];
[R, I] = max(A);
assert_isequal(R, -1-5i);
assert_isequal(I, 3);
%=============================================================================
A = [2 8 4; 7 3 9];
R = max(A);
assert_isequal(R, [7 8 9]);
%=============================================================================
A = [1.7 1.2 1.5; 1.3 1.6 1.99];
R = max(A, [], 2);
assert_isequal(R, [1.7; 1.99]);
%=============================================================================
A = [1 9 -2; 8 4 -5];
[R, I] = max(A);
assert_isequal(R, [8 9 -2]);
assert_isequal(I, [2 1 1]);
%=============================================================================
A = [1 7 3; 6 2 9];
B = 5;
R = max(A, B);
REF = [5 7 5; 6 5 9];
assert_isequal(R, REF);
%=============================================================================
A = [8 2 4; 7 3 9];
[R, I] = max(A(:));
assert_isequal(R, 9);
assert_isequal(I, 6);
%=============================================================================
A = 'Nelson';
[R, I] = max(A);
assert_isequal(R, 115);
assert_isequal(I, 4);
%=============================================================================
