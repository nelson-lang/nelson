%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('system'), 1);
assert_isequal(nargout('system'), 2);
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_exe = ['"', binpath, '/nelson-cli', '"'];
nelson_cmd = 'a=35;exit(a);';
cmd = [nelson_exe, ' --execute "', nelson_cmd, '"'];
[a, b] = system(cmd);
assert_isequal(a, 35);
%=============================================================================
