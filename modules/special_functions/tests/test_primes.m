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
assert_isequal(nargin('primes'), 1);
assert_isequal(nargout('primes'), 1);
%=============================================================================
n = uint16(12);
R = primes(n);
REF = uint16([2    3    5    7   11]);
assert_isequal(R, REF);
%=============================================================================
R = primes(25);
REF = [ 2     3     5     7    11    13    17    19    23];
assert_isequal(R, REF);
%=============================================================================
R = primes(3.3);
REF = [ 2     3];
assert_isequal(R, REF);
%=============================================================================
R = primes(single(3.3));
REF = single([ 2     3]);
assert_isequal(R, REF);
%=============================================================================
R = primes(-1);
REF = zeros(1, 0);
assert_isequal(R, REF);
%=============================================================================
R = primes(single(-1));
REF = zeros(1, 0, 'single');
assert_isequal(R, REF);
%=============================================================================
