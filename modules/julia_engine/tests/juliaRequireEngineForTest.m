%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function juliaRequireEngineForTest()
  env = jlenv();
  if strlength(env.Executable) == 0 || strlength(env.Library) == 0
    skip_testsuite(true, 'Julia engine not available')
  end
end
%=============================================================================
