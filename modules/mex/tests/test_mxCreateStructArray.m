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
if exist('mxCreateStructArray') == 0
    test_dir = [tempdir(), 'mxCreateStructArray'];
    if isdir(test_dir)
        rmdir(test_dir,'s');
    end
    mkdir(test_dir);
    status = copyfile('mxCreateStructArray.c', test_dir);
    assert_istrue(status);
    cd(test_dir);
    mex('mxCreateStructArray.c');
    addpath(pwd())
end
%=============================================================================
R = mxCreateStructArray();
assert_istrue(isstruct(R));
assert_isequal(size(R), [4 1]);
%=============================================================================
assert_isequal(R(1).fullname, 'Michael B.');
assert_isequal(R(1).date, 11122016);
%=============================================================================
assert_isequal(R(2).fullname, 'Pierre P.');
assert_isequal(R(2).date, 11122017);
%=============================================================================
assert_isequal(R(3).fullname, 'Nicolas M.');
assert_isequal(R(3).date, 11122018);
%=============================================================================
assert_isequal(R(4).fullname, 'Manu T.');
assert_isequal(R(4).date, 11122019);
%=============================================================================
