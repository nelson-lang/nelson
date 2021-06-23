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
assert_isequal(nargin('var'), 3);
assert_isequal(nargout('var'), 1);
%=============================================================================
R = var([1,2,3], 0, 1);
REF = [NaN NaN NaN];
assert_isequal(R, REF);
%=============================================================================
A = [4 -7 3; 1 4 -2; 10 7 9];
R = var(A);
REF = [21.0000   54.3333   30.3333];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [4 -2 1; 9 5 7];
R = var(A, 0, 1);
REF = [12.5000   24.5000   18.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [4 -2 1; 9 5 7];
R = var(A, 0, 2);
REF = [9; 4];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
assert_checkerror('var()', _('Wrong number of input arguments.'))
%=============================================================================
