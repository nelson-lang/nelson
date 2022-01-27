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
format('shortEng')
%=============================================================================
A = complex(rand(3,3,2), rand(3,3,2));
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

  Columns 1 through 2

     0.0000e+000 +  0.0000e+000i   913.3759e-003 + 35.7117e-003i
   905.7919e-003 +959.4924e-003i   632.3592e-003 +849.1293e-003i
   126.9868e-003 +655.7407e-003i    97.5404e-003 +933.9932e-003i

  Column 3

   278.4982e-003 +678.7352e-003i
   546.8815e-003 +757.7401e-003i
   957.5068e-003 +743.1325e-003i


A(:,:,2) =

  Columns 1 through 2

   964.8885e-003 +392.2270e-003i   957.1669e-003 +706.0461e-003i
   157.6131e-003 +655.4779e-003i   485.3756e-003 + 31.8328e-003i
   970.5928e-003 +171.1867e-003i   800.2805e-003 +276.9230e-003i

  Column 3

   141.8863e-003 + 46.1714e-003i
   421.7613e-003 + 97.1318e-003i
   915.7355e-003 +823.4578e-003i

';
assert_isequal(R, REF)
%=============================================================================
