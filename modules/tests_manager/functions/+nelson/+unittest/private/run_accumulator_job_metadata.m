%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function metadata = run_accumulator_job_metadata(accumulator, nativeIndex)
  metadata = cell(length(nativeIndex), 1);
  for k = 1:length(nativeIndex)
    metadata{k} = accumulator(nativeIndex(k)).filename;
  end
end
%=============================================================================
