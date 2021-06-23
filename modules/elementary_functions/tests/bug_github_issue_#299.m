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
% https://github.com/Nelson-numerical-software/nelson/issues/299
% <-- Short Description -->
% extends complex to manage sparse matrix
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
a = sparse(eye(3,3));
b = complex(a);
assert_istrue(isreal(a));
assert_isfalse(isreal(b));
assert_istrue(isequal(a, b));
%=============================================================================
a = sparse(eye(3,3) + i);
b = complex(a);
assert_isfalse(isreal(a));
assert_isfalse(isreal(b));
assert_isequal(a, b);
%=============================================================================
a = sparse(logical(eye(3,3)));
assert_checkerror('b = complex(a);', 'Undefined function ''sparselogical_complex''');
%=============================================================================