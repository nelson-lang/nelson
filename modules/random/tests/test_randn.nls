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
rng('default');
r = rng();
R = randn(5);
REF = [-0.9178     -0.6651      0.8356      0.8535      1.6081;
1.7324      1.4378      1.3359     -0.9093      0.5326;
-0.2969     -0.2262      0.1604     -1.7988     -1.0571;
-0.6355      0.2392     -0.4187      0.4198     -1.2411;
0.3646     -0.9054      0.1613     -1.0280      0.4881];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
rng('default');
R = randn(3, 2, 'single');
REF = [-0.9178     -0.6355;
1.7324      0.3646;
-0.2969     -0.6651];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
