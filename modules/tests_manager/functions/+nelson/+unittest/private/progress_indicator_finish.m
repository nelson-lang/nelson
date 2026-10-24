%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function progress_indicator_finish(progress, current)
  if ~progress.enabled
    return
  end
  if nargin < 2
    current = progress.total;
  end
  if isfield(progress, 'transient') && progress.transient
    % Erase the whole indicator line so it does not stay between test
    % results; the scheduling phase is transient progress, not a result.
    visibleLength = 2 + length(progress.label) + 2 + length(progress.previous);
    if visibleLength > 0
      eraser = repmat(sprintf('\b'), 1, visibleLength);
      blanks = repmat(' ', 1, visibleLength);
      fprintf(stdout, '%s%s%s', eraser, blanks, eraser);
    end
    return
  end
  progress_indicator_update(progress, current);
  fprintf(stdout, newline);
end
%=============================================================================
