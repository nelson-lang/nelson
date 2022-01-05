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
format('short')
%=============================================================================
A = complex(rand(3,3,2), rand(3,3,2));
A(1) = 0;
R = evalc('A');
REF = '
A(:,:,1) =

   0.0000 + 0.0000i   0.9134 + 0.0357i   0.2785 + 0.6787i
   0.9058 + 0.9595i   0.6324 + 0.8491i   0.5469 + 0.7577i
   0.1270 + 0.6557i   0.0975 + 0.9340i   0.9575 + 0.7431i


A(:,:,2) =

   0.9649 + 0.3922i   0.9572 + 0.7060i   0.1419 + 0.0462i
   0.1576 + 0.6555i   0.4854 + 0.0318i   0.4218 + 0.0971i
   0.9706 + 0.1712i   0.8003 + 0.2769i   0.9157 + 0.8235i

';
assert_isequal(R, REF)
%=============================================================================
