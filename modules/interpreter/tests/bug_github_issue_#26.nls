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
% https://github.com/Nelson-numerical-software/nelson/issues/26
% <-- Short Description -->
% 7.853981633974482790D-01 was not correctly parsed.
%=============================================================================
A = 7.853981633974482790D-01;
REF = 7.853981633974482790E-01;
assert_isequal(A, REF);
%=============================================================================
A = 7.853981633974482790d-01;
REF = 7.853981633974482790e-01;
assert_isequal(A, REF);
%=============================================================================
A = 7.853981633974482790d-01i;
REF = 7.853981633974482790e-01i;
assert_isequal(A, REF);
%=============================================================================
