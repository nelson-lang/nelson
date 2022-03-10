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
str = '2.7183  3.1416  0.0073';
R = sscanf(str, '%f');
REF = [2.7183; 3.1416; 0.0073];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
str = "2.7183  3.1416  0.0073";
R = sscanf(str, '%f', [1 3]);
REF = [2.7183    3.1416    0.0073];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
str = "2.7183  3.1416  0.0073";
R = sscanf(str,'%f',[2 2]);
REF = [ 2.7183    0.0073; 3.1416         0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
T = '78°F 72°F 64°F 66°F 49°F';
R = sscanf(T, '%d°F');
REF = [78; 72; 64; 66; 49];
assert_isequal(R, REF);
%=============================================================================
str = '3.14159 are the first 6 digits of pi';
[R, n, errmsg] = sscanf(str,'%f');
REF = 3.1416;
n_REF = 1;
errmsg_REF = _('Matching failure in format.');
assert_isapprox(R, REF, 1e-4);
assert_isequal(n, n_REF);
assert_isequal(errmsg, errmsg_REF);
%=============================================================================
[A, n, errmsg, nextindex] = sscanf(str,'%f');
assert_isequal(str(nextindex:end), 'are the first 6 digits of pi')
%=============================================================================
str = '⛔3.1416⛔';
[A,n,errmsg,nextindex] = sscanf(str,'%s%f%s')
%=============================================================================
