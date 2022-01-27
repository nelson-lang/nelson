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
A = complex(rand(3,3,2), rand(3,3,2));
A(1) = 0;
R = evalc('A');
REF =  '
A(:,:,1) =

  Column 1

    0.00000000000000e+000 + 0.00000000000000e+000i
    905.791934113950e-003 + 959.492425201461e-003i
    126.986811868846e-003 + 655.740696005523e-003i

  Column 2

    913.375855656341e-003 + 35.7116793747991e-003i
    632.359249982983e-003 + 849.129305453971e-003i
    97.5404016207904e-003 + 933.993245707825e-003i

  Column 3

    278.498218394816e-003 + 678.735158173367e-003i
    546.881519025192e-003 + 757.740125525743e-003i
    957.506829639897e-003 + 743.132471106946e-003i


A(:,:,2) =

  Column 1

    964.888533810154e-003 + 392.227019648999e-003i
    157.613076502457e-003 + 655.477893538773e-003i
    970.592778874561e-003 + 171.186689753085e-003i

  Column 2

    957.166949752718e-003 + 706.046083709225e-003i
    485.375648364425e-003 + 31.8328444845974e-003i
    800.280473195016e-003 + 276.922982186079e-003i

  Column 3

    141.886345110834e-003 + 46.1713904514909e-003i
    421.761285746470e-003 + 97.1317759249359e-003i
    915.735523682088e-003 + 823.457824299112e-003i

';
assert_isequal(R, REF)
%=============================================================================
