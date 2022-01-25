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
format('longE')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('single(A)');
REF =  '
  3×3×2 single array

ans(:,:,1) =

               0   9.1337585e-01   2.7849823e-01
   9.0579194e-01   6.3235927e-01   5.4688150e-01
   1.2698682e-01   9.7540401e-02   9.5750684e-01


ans(:,:,2) =

   9.6488851e-01   9.5716697e-01   1.4188634e-01
   1.5761308e-01   4.8537564e-01   4.2176127e-01
   9.7059280e-01   8.0028045e-01   9.1573554e-01

';
assert_isequal(R, REF)
%=============================================================================
