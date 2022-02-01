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
format('long')
%=============================================================================
R = evalc('A = single(complex(magic(3)*eps, eps))');
REF = '
A =

  3×3 single matrix

   1.0e-14 *

  0.1776357 + 0.0222045i  0.0222045 + 0.0222045i  0.1332268 + 0.0222045i
  0.0666134 + 0.0222045i  0.1110223 + 0.0222045i  0.1554312 + 0.0222045i
  0.0888178 + 0.0222045i  0.1998401 + 0.0222045i  0.0444089 + 0.0222045i

';
assert_isequal(R, REF);
%=============================================================================
