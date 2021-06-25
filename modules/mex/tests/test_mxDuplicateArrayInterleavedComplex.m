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
if ~isbuiltin('mxDuplicateArrayInterleavedComplex')
    test_dir = [tempdir(), 'mxDuplicateArray_interleaved'];
    if isdir(test_dir)
        rmdir(test_dir,'s');
    end
    mkdir(test_dir);
    status = copyfile('mxDuplicateArray.c', test_dir);
    assert_istrue(status);
    cd(test_dir);
    mex('-output', 'mxDuplicateArrayInterleavedComplex', '-R2018a', 'mxDuplicateArray.c' );
    run('loader.m');
end
%=============================================================================
% interleaved complex representation
i = [800 1100 20];
j = [900 1000 30];
v = complex([10 100 20], 1);
S = sparse(i, j, v, 1300, 1500, 30);
R = mxDuplicateArrayInterleavedComplex(S);
assert_isequal(R, S);
assert_isequal(nzmax(R), nzmax(S));
%=============================================================================
