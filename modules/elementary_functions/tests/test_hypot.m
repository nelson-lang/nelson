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
assert_isequal(nargin('hypot'), 2)
assert_isequal(nargout('hypot'), 1)
%=============================================================================
R = hypot(1, 2);
REF = 2.2361;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = hypot(single(1), 2);
REF = single(2.2361);
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = hypot(1, single(2));
REF = single(2.2361);
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = hypot(NaN, Inf);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = hypot(NaN, -Inf);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = hypot(Inf, NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = hypot(-Inf, NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = hypot(NaN, NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
A = 1e308;
R = hypot(A, A);
assert_isfalse(isinf(R));
%=============================================================================
A = [1:5];
B = [6:10];
R = hypot(A, B);
REF = [ 6.0828    7.2801    8.5440    9.8489   11.1803];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = hypot(3, 4);
REF = 5;
assert_isequal(R, REF);
%=============================================================================
R = hypot(1e309, 1e309);
assert_istrue(isinf(R));
%=============================================================================
R = hypot(ones(3, 0), 3);
REF = ones(3, 0);
assert_isequal(R, REF);
%=============================================================================
R = hypot(3, ones(3, 0));
REF = ones(3, 0);
assert_isequal(R, REF);
%=============================================================================
