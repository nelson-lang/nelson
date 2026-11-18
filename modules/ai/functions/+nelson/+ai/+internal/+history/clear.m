%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Forget a surface's stored conversation (deletes its history file). A missing
% file is not an error, so the pane's "clear conversation" always succeeds.
function clear(surfaceId)
  narginchk(1, 1);
  nargoutchk(0, 0);
  f = historyFile(surfaceId);
  if isfile(f)
    try
      delete(f);
    catch
    end
  end
end
%=============================================================================
