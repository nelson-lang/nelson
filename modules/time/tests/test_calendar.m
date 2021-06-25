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
assert_isequal(nargin('calendar'), 2);
assert_isequal(nargout('calendar'), 1);
%=============================================================================
c = calendar(1973, 8);
REF = [     0     0     0     1     2     3     4;
5     6     7     8     9    10    11;
12    13    14    15    16    17    18;
19    20    21    22    23    24    25;
26    27    28    29    30    31     0;
0     0     0     0     0     0     0];
assert_isequal(c, REF);
%=============================================================================
c = calendar(datenum(1973, 8, 4));
assert_isequal(c, REF);
%=============================================================================
