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
assert_isequal(nargin('num2bin'), 1)
assert_isequal(nargout('num2bin'), 1)
%=============================================================================
A = ['0011111111110000000000000000000000000000000000000000000000000000';
'0000000000000000000000000000000000000000000000000000000000000000';
'0000000000000000000000000000000000000000000000000000000000000000';
'0011111111110000000000000000000000000000000000000000000000000000';
'0000000000000000000000000000000000000000000000000000000000000000';
'0000000000000000000000000000000000000000000000000000000000000000'];
R = bin2num(A);
assert_isequal(R, [1; 0; 0; 1; 0; 0]);
%=============================================================================
assert_isequal(bin2num(num2bin(Inf)), Inf)
assert_isequal(bin2num(num2bin(-Inf)), -Inf)
%=============================================================================
assert_isequal(bin2num(num2bin(NaN)), NaN)
assert_isequal(bin2num(num2bin(-NaN)),  -NaN)
%=============================================================================
assert_isequal(bin2num(num2bin(single(Inf))), single(Inf))
assert_isequal(bin2num(num2bin(single(-Inf))), single(-Inf))
%=============================================================================
assert_isequal(bin2num(num2bin(single(NaN))), single(NaN))
assert_isequal(bin2num(num2bin(single(-NaN))), single(-NaN))
%=============================================================================
assert_isapprox(bin2num(num2bin(pi)), pi, 1e-9)
assert_isapprox(bin2num(num2bin(single(pi))), single(pi), 1e-6)
%=============================================================================

