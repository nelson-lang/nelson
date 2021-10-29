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
p = [30 0 -20 0 10 50];
q = polyder(p);
REF = [ 150     0   -60     0    10];
assert_isequal(q, REF);
%=============================================================================
a = [1 -2 0 0 11];
b = [1 -10 15];
q = polyder(a, b);
REF = [  6   -60   140   -90    22  -110];
assert_isequal(q, REF);
%=============================================================================
q = polyder(33);
REF = 0;
assert_isequal(q, REF);
%=============================================================================
