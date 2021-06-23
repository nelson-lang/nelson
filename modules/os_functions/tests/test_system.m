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
assert_isequal(nargin('system'), 1);
assert_isequal(nargout('system'), 2);
%=============================================================================
binpath = modulepath(nelsonroot,'core','bin');
nelson_exe = ['"', binpath, '/nelson-cli', '"'];
nelson_cmd = 'a=35;exit(a);';
cmd = [nelson_exe, ' --execute "', nelson_cmd, '"'];
[a, b] = system(cmd);
assert_isequal(a, 35);
%=============================================================================
[s, w] = system(["echo hello", "echo my", "echo world"], '-parallel');
assert_isequal(s, [0 0 0]);
assert_isequal(size(w), [1 3]);
assert_istrue(isa(w, 'string'));
assert_istrue(startsWith(w{1}, 'hello'));
assert_istrue(startsWith(w{2}, 'my'));
assert_istrue(startsWith(w{3}, 'world'));
%=============================================================================
