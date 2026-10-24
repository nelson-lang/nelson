%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = use_inline_unittest_runner()
  if strcmp(getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER'), 'TRUE')
    tf = false;
    return
  end
  executionMode = upper(getenv('NELSON_UNITTEST_EXECUTION_MODE'));
  if strcmp(executionMode, 'INLINE')
    tf = true;
  elseif strcmp(executionMode, 'PROCESS')
    tf = false;
  else
    tf = ~canUseNativeRunner() || is_unittest_runfile_worker();
  end
end
%=============================================================================
