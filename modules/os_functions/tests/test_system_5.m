%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Native nelson-cli timeout option is not available in the portable runtime.');
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_exe = ['"', binpath, '/nelson-cli', '"'];
else
  nelson_exe = [binpath, '/nelson-cli'];
end
cmd = [nelson_exe, ' --timeout 4 --execute "pause(60)"'];
tic(); [s, m] = system(cmd); R = toc();
if ispc()
  asserts.isequal(s, 258);
else
  asserts.isequal(s, 134);
end
asserts.istrue(R > 4);
asserts.istrue(R < 12);
%=============================================================================
