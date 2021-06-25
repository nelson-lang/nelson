%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/68
% <-- Short Description -->
% display of nd array of integer were not defined.
%=============================================================================
% <--CHECK REF-->
% <--ENGLISH IMPOSED-->
%=============================================================================
repmat(int8([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(int16([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(int32([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(int64([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(uint8([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(uint16([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(uint32([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
repmat(uint64([1 0 1;0 1 0]), 2, 3, 2)
%=============================================================================
int8(eye(3, 3))
%=============================================================================
int16(eye(3, 3))
%=============================================================================
int32(eye(3, 3))
%=============================================================================
int64(eye(3, 3))
%=============================================================================
uint8(eye(3, 3))
%=============================================================================
uint16(eye(3, 3))
%=============================================================================
uint32(eye(3, 3))
%=============================================================================
uint64(eye(3, 3))
%=============================================================================
