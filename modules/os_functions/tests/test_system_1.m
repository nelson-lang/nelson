%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('system'), 1);
asserts.isequal(nargout('system'), 2);
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Native nelson-cli exit-code propagation is not available in the portable runtime.');
binpath = modulepath('nelson', 'bin');
nelson_exe = ['"', binpath, '/nelson-cli', '"'];
nelson_cmd = 'a=35;exit(a);';
cmd = [nelson_exe, ' --execute "', nelson_cmd, '"'];
[a, b] = system(cmd);
asserts.isequal(a, 35);
%=============================================================================
