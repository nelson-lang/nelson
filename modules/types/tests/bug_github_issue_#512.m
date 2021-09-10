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
% https://github.com/Nelson-numerical-software/nelson/issues/512
% <-- Short Description -->
% Assign must not change left assign type when it is possible
%=============================================================================
A = single(eye(3,3));
A(1,1) = 'f';
assert_isequal(class(A), 'single')
A(2,2) = int32(4);
assert_isequal(class(A), 'single')
A(3,3) = 3;
assert_isequal(class(A), 'single')
REF = single([102, 0, 0; 0 4 0; 0 0 3]);
assert_isequal(A, REF)
A(1,:) = [0 1 0];
assert_isequal(class(A), 'single')
%=============================================================================