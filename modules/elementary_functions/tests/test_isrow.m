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
assert_isequal(nargin('isrow'), 1)
assert_isequal(nargout('isrow'), 1)
%=============================================================================
s.c = 1;
assert_istrue(isrow(s))
%=============================================================================
assert_istrue(isrow("n"))
assert_istrue(isrow('n'))
assert_istrue(isrow("nelson"))
assert_istrue(isrow('nelson'))
assert_isfalse(isrow(["test"; "ing"]))
assert_istrue(isrow(["test", "ing"]))
%=============================================================================
assert_istrue(isrow ([1, 2, 3]))
assert_isfalse(isrow ([1; 2; 3]))
assert_istrue(isrow (1))
assert_isfalse(isrow ([]))
assert_isfalse(isrow ([1, 2; 3, 4]))
%=============================================================================

