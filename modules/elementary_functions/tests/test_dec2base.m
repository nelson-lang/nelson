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
assert_isequal(nargin('dec2base'), 3)
assert_isequal(nargout('dec2base'), 1)
%=============================================================================
R = dec2base([20,140], 10);
REF = ['020'; '140'];
assert_isequal(R, REF);
%=============================================================================
V = 1:100;
R = base2dec(dec2base(V, 2), 2);
assert_isequal(R, V');
%=============================================================================
V = 1:100;
for b = [2:36]
  R = base2dec(dec2base(V, b), b);
  assert_isequal(R, V');
end
%=============================================================================
