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
assert_isequal(nargin('any'), 2)
assert_isequal(nargout('any'), 1)
%=============================================================================
assert_isequal(any([]), false)
assert_isequal(any([], 1), logical(zeros(1, 0)))
assert_isequal(any([], 2), logical(zeros(0, 1)))
assert_isequal(any([], 3), logical(zeros(0, 0)))
%=============================================================================
assert_isequal(any(ones(1, 0)), false)
assert_isequal(any(ones(0, 1)), false)
%=============================================================================
assert_isequal(any(ones(0, 0, 0)), logical(zeros(1, 0, 0)))
%=============================================================================
assert_isequal(any(ones(0, 2)), [false false])
assert_isequal(any(ones(3,0)), logical(ones(1, 0)))
assert_isequal(any(ones(3, 0, 1)), logical(ones(1, 0)))
%=============================================================================
assert_isequal(any([0 0 3;0 0 3;0 0 3]), logical([0   0   1]))
%=============================================================================
assert_isequal(any([0 0 3;0 0 3;0 0 3], 2), logical([1;   1;   1]))
%=============================================================================
