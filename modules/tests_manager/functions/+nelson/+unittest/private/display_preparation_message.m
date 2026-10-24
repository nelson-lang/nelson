%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function display_preparation_message(phase, count, nbWorkers)
  if count < 50
    return
  end
  if strcmp(phase, 'prepare')
    if progress_indicator_enabled(count)
      return
    end
    fprintf(stdout, ['  ', _('Preparing %d tests for %d workers.'), newline], ...
      count, nbWorkers);
  elseif strcmp(phase, 'run')
    fprintf(stdout, ['  ', _('Running %d jobs; results are displayed in test order.'), ...
      newline], count);
  end
end
%=============================================================================
