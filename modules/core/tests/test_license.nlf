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
assert_isequal(nargin('license'), -1);
assert_isequal(nargout('license'), -1);
%=============================================================================
license();
%=============================================================================
a = license();
assert_istrue(strcmp(a, 'GNU Lesser General Public License v2.1') || strcmp(a, 'GNU General Public License v2'))
%=============================================================================
[a, b] = license();
assert_istrue(strcmp(a, 'GNU Lesser General Public License v2.1') || strcmp(a, 'GNU General Public License v2'))
assert_istrue(ischar(b));
assert_istrue(length(b) > 100);
%=============================================================================