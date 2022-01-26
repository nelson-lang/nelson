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
format('longG')
%=============================================================================
A = complex(rand(3,3,2), rand(3,3,2));
A(1) = 0;
R = evalc('A');
REF =   '
A(:,:,1) =

  Column 1

                          0 +                     0i
           0.90579193411395 +     0.959492425201461i
          0.126986811868846 +     0.655740696005523i

  Column 2

          0.913375855656341 +    0.0357116793747991i
          0.632359249982983 +     0.849129305453971i
         0.0975404016207904 +     0.933993245707825i

  Column 3

          0.278498218394816 +     0.678735158173367i
          0.546881519025192 +     0.757740125525743i
          0.957506829639897 +     0.743132471106946i


A(:,:,2) =

  Column 1

          0.964888533810154 +     0.392227019648999i
          0.157613076502457 +     0.655477893538773i
          0.970592778874561 +     0.171186689753085i

  Column 2

          0.957166949752718 +     0.706046083709225i
          0.485375648364425 +    0.0318328444845974i
          0.800280473195016 +     0.276922982186079i

  Column 3

          0.141886345110834 +    0.0461713904514909i
           0.42176128574647 +    0.0971317759249359i
          0.915735523682088 +     0.823457824299112i

';
assert_isequal(R, REF)
%=============================================================================
