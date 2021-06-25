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
if ~isbuiltin('mexAtExit')
    status = copyfile([modulepath('mex'), '/tests/', 'mexAtExit.c'], tempdir());
    assert_istrue(status);
    cd(tempdir());
    mex('mexAtExit.c');
    run('loader.m');
end
%=============================================================================
mexAtExit();
R = evalc('clear mex');
assert_isequal(R, 'Call at Exit');
R = evalc('clear mexAtExit');
assert_isequal(R, 'Call at Exit');
R = evalc('clear all');
assert_isequal(R, 'Call at Exit');

%=============================================================================
