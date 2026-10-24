%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function worker_pool_cleanup_stale_scripts(maxAgeDays, rootDirInput)
  if nargin == 0
    ageDays = worker_pool_stale_script_age_days();
  else
    ageDays = maxAgeDays;
  end
  if nargin < 2
    rootDir = unittest_tempdir();
  else
    rootDir = rootDirInput;
  end
  files = dir([rootDir, 'nelson_unittest_worker_*.m']);
  threshold = now() - ageDays;
  for k = 1:length(files)
    if ~files(k).isdir && files(k).datenum < threshold
      run_removeFile([rootDir, files(k).name]);
    end
  end
end
%=============================================================================
