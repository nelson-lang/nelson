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
A = rand(3,3,2);
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

            0   9.1338e-01   2.7850e-01
   9.0579e-01   6.3236e-01   5.4688e-01
   1.2699e-01   9.7540e-02   9.5751e-01


A(:,:,2) =

   9.6489e-01   9.5717e-01   1.4189e-01
   1.5761e-01   4.8538e-01   4.2176e-01
   9.7059e-01   8.0028e-01   9.1574e-01

';
assert_isequal(R, REF)
%=============================================================================
