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
x = linspace(0, 4 * pi, 10);
y = sin(x);
p = polyfit(x, y, 7);
REF = [-0.0001    0.0028   -0.0464    0.3702   -1.3808    1.9084   -0.1141    0.0002];
assert_isapprox(p, REF, 1e-4);
%=============================================================================
x = [1, 2, 3; 4, 5, 6];
y = [0, 0, 1; 1, 0, 0];
p = polyfit (x, y, 5);
REF = [ 0.0000    0.0833   -1.1667    5.4167   -9.3333    5.0000];
assert_isapprox(p, REF, 1e-4);
%=============================================================================
