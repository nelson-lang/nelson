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
assert_isequal(nargin('cosd'), 1);
assert_isequal(nargout('cosd'), 1);
%=============================================================================
assert_isequal(cosd(NaN), NaN);
assert_isequal(cosd(-NaN), NaN);
assert_isequal(cosd(Inf), NaN);
assert_isequal(cosd(-Inf), NaN);
%=============================================================================
assert_isequal(cosd ([90, 270]), [0 0]);
assert_isequal(cosd([0, 180, 360]), [1 -1  1]);
assert_isapprox(cosd(0:10:90), cos (pi*[0:10:90]* inv(180)), -10 * eps);
%=============================================================================
