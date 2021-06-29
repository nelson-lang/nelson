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
assert_isequal(nargin('conv'), 3);
assert_isequal(nargout('conv'), 1);
%=============================================================================
U = [1 0 1];
V = [2 7];
R = conv(U, V);
REF = [ 2     7     2     7];
assert_isequal(R, REF);
%=============================================================================
U = [1 1 1];
V = [1 1 0 0 0 1 1];
R = conv(U, V);
REF = [1     2     2     1     0     1     2     2     1];
assert_isequal(R, REF);
%=============================================================================
U = [-1 2 3 -2 0 1 2];
V = [2 4 -1 1];
R = conv(U, V, 'same');
REF = [15     5    -9     7     6     7    -1];
assert_isequal(R, REF);
%=============================================================================
