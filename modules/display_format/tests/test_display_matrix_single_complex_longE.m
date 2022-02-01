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
format('longE')
%=============================================================================
rng('default')
A = complex(rand(2, 2), 1);
A(2, 2) = NaN;
R = evalc('A = single(A)');
REF = '
A =

  2×2 single matrix

  8.1472367e-01 + 1.0000000e+00i  1.2698682e-01 + 1.0000000e+00i
  9.0579194e-01 + 1.0000000e+00i            NaN + 0.0000000e+00i

';
assert_isequal(R, REF)
%=============================================================================
