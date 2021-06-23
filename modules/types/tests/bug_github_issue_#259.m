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
% https://github.com/Nelson-numerical-software/nelson/issues/259
% <-- Short Description -->
% extraction decomplexify values
%=============================================================================
A = [1 0 0 0;
0 2 0 0;
0 0 3 0;
0 0 0 4];
CX = sparse (complex(A));
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(6)));
%=============================================================================
CX = complex(1);
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(1)));
%=============================================================================
CX = single(complex(1));
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(1)));
%=============================================================================
A = rand(3, 2, 3);
CX = complex(A);
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(1:3)));
assert_istrue(isreal(CX(1, 2, 2)));
%=============================================================================
A = rand(3, 2);
CX = complex(A);
assert_isfalse(isreal(CX));
assert_istrue(isreal(CX(2, 1)));
assert_istrue(isreal(CX(1:2, 1:2)));
%=============================================================================
