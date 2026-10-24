%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function progressive_cleanup_scripts(testCases, worker)
  run_removeFile(worker.command_filename);
  for k = 1:length(testCases)
    run_removeFile(testCases(k).command_filename);
  end
end
%=============================================================================
