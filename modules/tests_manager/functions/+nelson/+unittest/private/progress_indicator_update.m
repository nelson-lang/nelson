%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function progress = progress_indicator_update(progress, current)
  if ~progress.enabled
    return
  end
  percent = round(100 * double(current) / progress.total);
  percent = min(100, max(0, percent));
  if percent == progress.last_percent
    return
  end
  text = sprintf('%d %%', percent);
  if isempty(progress.previous)
    fprintf(stdout, ['  ', '%s: %s'], progress.label, text);
  else
    backspace = repmat(sprintf('\b'), 1, length(progress.previous));
    fprintf(stdout, '%s%s', backspace, text);
  end
  progress.previous = text;
  progress.last_percent = percent;
end
%=============================================================================
