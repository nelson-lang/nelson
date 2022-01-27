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
format('longEng')
%=============================================================================
A = rand(3,3,2);
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

    0.00000000000000e+000    913.375855656341e-003    278.498218394816e-003
    905.791934113950e-003    632.359249982983e-003    546.881519025192e-003
    126.986811868846e-003    97.5404016207904e-003    957.506829639897e-003


A(:,:,2) =

    964.888533810154e-003    957.166949752718e-003    141.886345110834e-003
    157.613076502457e-003    485.375648364425e-003    421.761285746470e-003
    970.592778874561e-003    800.280473195016e-003    915.735523682088e-003

';
assert_isequal(R, REF)
%=============================================================================
