%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = progress_indicator_enabled(count)
  if nargin < 1
    count = 0;
  end
  if count < 50
    tf = false;
    return
  end
  if strcmp(getenv('NELSON_UNITTEST_PROGRESS_ANIMATION'), '1')
    tf = true;
    return
  end
  mode = getnelsonmode();
  tf = strcmp(mode, 'GUI') || strcmp(mode, 'WEB_GUI') || strcmp(mode, 'WASM_TERMINAL');
end
%=============================================================================
