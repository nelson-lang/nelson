%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Native nelson-cli exit-code propagation is not available in the portable runtime.');
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_exe = ['"', binpath, '/nelson-cli', '"'];
else
  nelson_exe = [binpath, '/nelson-cli'];
end
%=============================================================================
cmd = [nelson_exe, ' -e "exit(88,''force'')"'];
[R, msg] = system(cmd);
asserts.isequal(R, 88);
%=============================================================================
