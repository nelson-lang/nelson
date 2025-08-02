%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
cmd = 'echo hello && echo world';
[a, b] = unix(cmd);
assert_isequal(a, 0);
if ispc()
  REF = sprintf('%s\n%s\n', 'hello ', 'world');
else
  REF = sprintf('%s\n%s\n', 'hello', 'world');
end
assert_isequal(b, REF);
%=============================================================================
