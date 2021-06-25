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
addpath([fileparts(nfilename('fullpathext'), 'path'), '/loadsavebin']);
%=============================================================================
% double
A = eye(5, 4);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% double complex
A = eye(5,4) + 2i;
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% double empty
A = ones(0,3);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array double
A = ones(3,4,2);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array double complex
A = complex(ones(3,4,2), 2);
savebin([tempdir(), 'test_saveload_double.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_double.bin']);
assert_isequal(A, REF);
%=============================================================================
