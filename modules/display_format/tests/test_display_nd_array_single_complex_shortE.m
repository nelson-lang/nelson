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
rng('default')
format('shortE')
%=============================================================================
A = complex(rand(3,3,2), rand(3,3,2));
A(1) = 0;
R = evalc('single(A)');
REF = '
  3×3×2 single array

ans(:,:,1) =

  0.0000e+00 +0.0000e+00i  9.1338e-01 +3.5712e-02i  2.7850e-01 +6.7874e-01i
  9.0579e-01 +9.5949e-01i  6.3236e-01 +8.4913e-01i  5.4688e-01 +7.5774e-01i
  1.2699e-01 +6.5574e-01i  9.7540e-02 +9.3399e-01i  9.5751e-01 +7.4313e-01i


ans(:,:,2) =

  9.6489e-01 +3.9223e-01i  9.5717e-01 +7.0605e-01i  1.4189e-01 +4.6171e-02i
  1.5761e-01 +6.5548e-01i  4.8538e-01 +3.1833e-02i  4.2176e-01 +9.7132e-02i
  9.7059e-01 +1.7119e-01i  8.0028e-01 +2.7692e-01i  9.1574e-01 +8.2346e-01i

';
assert_isequal(R, REF)
%=============================================================================
