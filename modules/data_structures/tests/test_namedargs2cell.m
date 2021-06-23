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
assert_isequal(nargin('namedargs2cell'), 1);
assert_isequal(nargout('namedargs2cell'), 1);
%=============================================================================
S = struct();
R = namedargs2cell(S);
REF = cell(1, 0);
assert_isequal(R, REF);
%=============================================================================
S = struct();
S.CharacterEncoding = 'auto';
S.Timeout = 5;
S.Username = "";
S.logical = false;
R = namedargs2cell(S);
REF = { 'CharacterEncoding', 'auto', 'Timeout', 5, 'Username', "", 'logical', false};
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = namedargs2cell(1);', _('Wrong type for argument #1. struct expected.'));
%=============================================================================
