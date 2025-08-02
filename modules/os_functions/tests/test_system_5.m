%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_exe = ['"', binpath, '/nelson', '"'];
else
  nelson_exe = [binpath, '/nelson'];
end
cmd = [nelson_exe, ' -cli --timeout 20'];
tic();[s,m]=system(cmd); R = toc();
if ispc()
  assert_isequal(s, 258);
else
  assert_isequal(s, 134);
end
assert_istrue(R > 20);
assert_istrue(R < 30);
%=============================================================================
