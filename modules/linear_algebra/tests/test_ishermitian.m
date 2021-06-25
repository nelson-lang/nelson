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
assert_isequal(nargin('ishermitian'), 2);
assert_isequal(nargout('ishermitian'), 1);
%=============================================================================
assert_istrue(ishermitian ([0, 2i; 2i, 0], 'skew'));
assert_isfalse(ishermitian ([0, 2i; 2i, 0], 'nonskew'));
%=============================================================================
assert_isfalse(ishermitian([1 0 1i; 0 1 0; 1i 0 1]));
assert_istrue(ishermitian([1 0 1i; 0 1 0; -1i 0 1]));
%=============================================================================
assert_istrue(ishermitian([-1i -1 1-i;1 -1i -1;-1-i 1 -1i], 'skew'));
assert_isfalse(ishermitian([-1i -1 1-i;1 -1i -1;-1-i 1 -1i], 'nonskew'));
%=============================================================================
assert_istrue(ishermitian(1));
assert_isfalse(ishermitian([1, 2]));
assert_istrue(ishermitian([]));
assert_istrue(ishermitian(NaN));
assert_istrue(ishermitian(Inf));
assert_istrue(ishermitian([1, 2; 2, 1]));
assert_isfalse(ishermitian([1, 2i; 2i, 1]));
%=============================================================================
A = [1 0 1i; 0 1 0;-1i 0 1];
assert_istrue(ishermitian(A));
assert_isfalse(ishermitian(A, 'skew'));
assert_istrue(ishermitian(A, 'nonskew'));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0];
assert_istrue(ishermitian(A, 'skew'));
assert_isfalse(ishermitian(A, 'nonskew'));
assert_isfalse(ishermitian(A));
%=============================================================================
A = [0 1 -2 5; -1 0 3 -4; 2 -3 0 6; -5 4 -6 0] * i;
assert_isfalse(ishermitian(A, 'skew'));
assert_istrue(ishermitian(A, 'nonskew'));
assert_istrue(ishermitian(A));
%=============================================================================
A = [0 NaN(); NaN() 0];
assert_isfalse(ishermitian(A, 'skew'));
assert_isfalse(ishermitian(A, 'nonskew'));
assert_isfalse(ishermitian(A));
%=============================================================================
A = [0 1; 1 0];
assert_isfalse(ishermitian(A, 'skew'));
assert_istrue(ishermitian(A, 'nonskew'));
assert_istrue(ishermitian(A));
%=============================================================================
A = single([0 1; 1 0]);
assert_isfalse(ishermitian(A, 'skew'));
assert_istrue(ishermitian(A, 'nonskew'));
assert_istrue(ishermitian(A));
%=============================================================================
A = int8([0 1; 1 0]);
assert_isfalse(ishermitian(A, 'skew'));
assert_istrue(ishermitian(A, 'nonskew'));
assert_istrue(ishermitian(A));
%=============================================================================
A = uint8([0 1; 1 0]);
assert_isfalse(ishermitian(A, 'skew'));
assert_istrue(ishermitian(A, 'nonskew'));
assert_istrue(ishermitian(A));
%=============================================================================
assert_isfalse(ishermitian(ones(3,3,3)));
%=============================================================================
assert_isfalse(ishermitian(-2i-9))
assert_isfalse(ishermitian(2i-9))
assert_isfalse(ishermitian(-2i))
assert_isfalse(ishermitian(2i))
assert_istrue(ishermitian(3))
%=============================================================================