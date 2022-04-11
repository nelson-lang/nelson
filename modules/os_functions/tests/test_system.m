%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
