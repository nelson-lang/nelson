%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function txt = report_legacyMessageText(msg)
  if isstruct(msg)
    txt = msg.message;
  elseif iscellstr(msg)
    txt = strjoin(msg, newline());
  else
    txt = msg;
  end
end
