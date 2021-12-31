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
assert_isequal(nargin('iscolumn'), 1)
assert_isequal(nargout('iscolumn'), 1)
%=============================================================================
s.c = 1;
assert_istrue(iscolumn(s))
%=============================================================================
assert_istrue(iscolumn("n"))
assert_istrue(iscolumn('n'))
assert_istrue(iscolumn("nelson"))
assert_isfalse(iscolumn('nelson'))
assert_istrue(iscolumn(["test"; "ing"]))
assert_isfalse(iscolumn(["test", "ing"]))
%=============================================================================
assert_isfalse(iscolumn([1, 2, 3]))
assert_istrue(iscolumn([1; 2; 3]))
assert_istrue(iscolumn(1))
assert_isfalse(iscolumn([]))
assert_isfalse(iscolumn([1, 2; 3, 4]))
%=============================================================================

