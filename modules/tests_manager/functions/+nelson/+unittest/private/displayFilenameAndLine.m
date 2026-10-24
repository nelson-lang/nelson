%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function displayFilenameAndLine(msg)
  if (length(msg.stack) > 0)
    pos = 1;
    if (msg.stack(1).line == 0)
      pos = 2;
      if ~((length(msg.stack) > 1) && (msg.stack(pos).line ~= 0))
        pos = -1;
      end
    end
    if pos > 0
      disp(['      ', _('File:'), ' ', msg.stack(pos).file]);
      disp(['      ', _('Line:'), ' ', num2str(msg.stack(pos).line)]);
    end
  end
end
