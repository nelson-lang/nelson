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
if ~isbuiltin('mxGetFieldByNumber')
    test_dir = [tempdir(), 'mxGetFieldByNumber'];
    if isdir(test_dir)
        rmdir(test_dir,'s');
    end
    mkdir(test_dir);
    status = copyfile('mxGetFieldByNumber.c', test_dir);
    assert_istrue(status);
    cd(test_dir);
    mex('mxGetFieldByNumber.c');
    run('loader.m');
end
%=============================================================================
C(1).A = 1;
C(1).B = single(2);
C(1).C = true;
C(2).A = 3;
C(2).B = single(4);
C(2).C = false;
R = mxGetFieldByNumber(C, 0, 0);
assert_isequal(R, 1);
R = mxGetFieldByNumber(C, 0, 1);
assert_isequal(R, single(2));
R = mxGetFieldByNumber(C, 0, 2);
assert_isequal(R, true);
R = mxGetFieldByNumber(C, 1, 0);
assert_isequal(R, 3);
R = mxGetFieldByNumber(C, 1, 1);
assert_isequal(R, single(4));
R = mxGetFieldByNumber(C, 1, 2);
assert_isequal(R, false);
R = mxGetFieldByNumber(C, 2, 2);
assert_isequal(R, false);
%=============================================================================
