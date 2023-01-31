%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('dos'), 1);
assert_isequal(nargout('dos'), 2);
%=============================================================================
binpath = modulepath(nelsonroot,'nelson','bin');
nelson_exe = ['"', binpath, '/nelson-cli', '"'];
%=============================================================================
nelson_cmd = 'exit(33);';
cmd = [nelson_exe, ' --execute "', nelson_cmd, '"'];
[a, b] = dos(cmd);
assert_isequal(a, 33);
%=============================================================================
