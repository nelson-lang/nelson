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
R = intmax('int8') .* 2;
R = intmax('int8') .* -2;
%=============================================================================
R = intmax('int8') .* intmax('int8');
REF = intmax('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmax('int8') .* intmin('int8');
REF = intmin('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmin('int8') .* intmax('int8');
REF = intmin('int8');
assert_isequal(R, REF);
%=============================================================================
R = intmin('int8') .* intmin('int8');
REF = intmax('int8');
assert_isequal(R, REF);
%=============================================================================
R = int8([1 2]) .* 3;
REF =  int8([3 6]);
assert_isequal(R, REF);
%=============================================================================
R = 3 .* int8([1 2]);
REF =  int8([3 6]);
assert_isequal(R, REF);
%=============================================================================
R = int8([1 2]) .* int8([3, 4]);
REF = int8([3 8]);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = single(3) .* int8([1 2]);', sprintf(_('function %s undefined.'), 'single_times_int8'));
assert_checkerror('R = int8([1 2]) .* single(3);', sprintf(_('function %s undefined.'), 'int8_times_single'));
%=============================================================================
