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
R = int32(1):4;
REF = int32([1 2 3 4]);
assert_isequal(R, REF);
%=============================================================================
R = 1:2:10;
REF = [1  3  5  7  9];
assert_isequal(R, REF);
%=============================================================================
R = 1:single(2):10;
REF = single([1  3  5  7  9]);
assert_isequal(R, REF);
%=============================================================================
R = 1:int64(3);
REF = int64([1:3]);
assert_isequal(R, REF);
%=============================================================================
R = 1:int32(2):4;
REF = int32([1 3]);
assert_isequal(R, REF);
%=============================================================================
R = 1:char(2):4;
RR = double(R);
REF = [1:2:4];
assert_isequal(RR, REF);
%=============================================================================
R = 1:1;
REF = 1;
assert_isequal(R, REF);
%=============================================================================
A = double(intmax('uint32'))-3;
B = intmax('uint32');
R = A:1:B;
REF = uint32([4294967292   4294967293   4294967294   4294967295]);
assert_isequal(R, REF);
%=============================================================================
R = 9:int32(-2):4;
REF =  int32([9   7   5]);
assert_isequal(R, REF);
%=============================================================================
R = 1:0:3;
REF = zeros(1, 0);
assert_isequal(R, REF);
%=============================================================================
R = 1:5;
REF = [1 2 3 4 5];
assert_isequal(R, REF);
%=============================================================================
R = 1:2:5;
REF = [1 3 5];
assert_isequal(R, REF);
%=============================================================================
R = 1:1:0;
REF = zeros(1, 0);
assert_isequal(R, REF);
%=============================================================================
% 5 > (1-0.9)*50
% WARNING it is same than 1:4.9999999
R = 1:(1-0.9)*50;
REF = [1 2 3 4];
assert_isequal(R, REF);
%=============================================================================
R = 1:5;
REF = [1 2 3 4 5];
assert_isequal(R, REF);
%=============================================================================
R = 1:(1 - 0.9) * 20:10;
REF = [  1.0000    3.0000    5.0000    7.0000    9.0000];
assert_isapprox(R,REF, 1e-4);
%=============================================================================
R = 10:-0.9:1;
REF = [10, 9.1, 8.2, 7.3, 6.4, 5.5, 4.6, 3.7, 2.8, 1.9, 1];
assert_isapprox(R,REF, 1e-5);
%=============================================================================
R = 1:1:NaN;
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = 1:NaN:2;
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = NaN:2:4;
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = inf:4;
REF = ones(1, 0);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = -inf:4;', _('Invalid range.'));
%=============================================================================
R = -inf:-inf:4;
REF = ones(1, 0);
assert_isequal(R, REF);
%=============================================================================
R = inf:-inf:4;
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = -inf:inf:4;
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = 3:inf:4;
REF = 3;
assert_isequal(R, REF);
%=============================================================================
R = 1:-inf:4;
REF = ones(1, 0);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = 1:1:inf;',  _('Invalid range.'));
%=============================================================================
assert_checkerror('single(1):int32(2):single(4)', _('Colon input arguments must have same type.'));
%=============================================================================
