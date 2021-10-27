%=============================================================================
% Copyright (c) 2017-2018 Allan CORNET (Nelson)
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
% https://github.com/Nelson-numerical-software/nelson/issues/534
% <-- Short Description -->
% diag(ones(0,1),-1) must return a value 0.
%=============================================================================
R = diag(ones(0, 1), -1);
REF = 0;
assert_isequal(R, REF)
%=============================================================================
R = diag(ones(0, 0), -1);
REF = [];
assert_isequal(R, REF)
%=============================================================================
R = diag(ones(1, 0), -1);
REF = 0;
assert_isequal(R, REF)
%=============================================================================
