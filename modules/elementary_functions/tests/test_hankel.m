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
assert_isequal(nargin('hankel'), 2)
assert_isequal(nargout('hankel'), 1)
%=============================================================================
c = [1 2 3 4];
R = hankel(c);
REF = [1     2     3     4;
2     3     4     0;
3     4     0     0;
4     0     0     0];
assert_isequal(R, REF);
%=============================================================================
c = [2 4 6];
r = [6 5 4 3 2 1];
R = hankel(c, r);
REF = [ 2     4     6     5     4     3;
4     6     5     4     3     2;
6     5     4     3     2     1];
assert_isequal(R, REF);
%=============================================================================
c = [1 2 3];
r = [4 5 7 9];
R = hankel(c, r);
REF = [1     2     3     5;
2     3     5     7;
3     5     7     9];
assert_isequal(R, REF);
msg = lastwarn();
assert_isequal(_('Last element of input column does not match first element of input row.'), msg)
%=============================================================================
