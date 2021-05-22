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
assert_isequal(nargin('setfield'), -2);
assert_isequal(nargout('setfield'), 1);
%=============================================================================
R = {};
R = setfield(R, 'toto', 3);
REF = struct('toto', 3);
assert_isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
R = setfield(R, 'titi', 4);
REF = struct('toto', 3, 'titi', 4);
assert_isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
R = setfield(R, "titi", 4);
REF = struct('toto', 3, 'titi', 4);
assert_isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
assert_checkerror('R = setfield(R, 19, 4);', _('Input should be a string or character array.'));
%=============================================================================
