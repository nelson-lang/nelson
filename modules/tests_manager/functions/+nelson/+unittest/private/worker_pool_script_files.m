%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function content = worker_pool_script_files(chunk)
  content = cell(length(chunk), 1);
  for k = 1:length(chunk)
    content{k} = ['worker_files = [worker_files; ', ...
      nelsonStringLiteral(chunk(k).filename), '];'];
  end
end
%=============================================================================
