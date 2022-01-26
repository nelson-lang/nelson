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
A = rand(3,3,2);
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

                         0         0.913375855656341         0.278498218394816
          0.90579193411395         0.632359249982983         0.546881519025192
         0.126986811868846        0.0975404016207904         0.957506829639897


A(:,:,2) =

         0.964888533810154         0.957166949752718         0.141886345110834
         0.157613076502457         0.485375648364425          0.42176128574647
         0.970592778874561         0.800280473195016         0.915735523682088

';
assert_isequal(R, REF)
%=============================================================================
