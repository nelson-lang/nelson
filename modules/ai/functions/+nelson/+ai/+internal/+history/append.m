%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Append one turn to a surface's conversation history and persist it. role is
% 'user' or 'assistant'; an empty text or unknown role is ignored. The stored
% history is capped (oldest turns dropped) so the file never grows without bound.
% A write failure (read-only prefs dir) is swallowed: history is best-effort and
% never blocks a reply.
function append(surfaceId, role, text)
  narginchk(3, 3);
  nargoutchk(0, 0);
  role = char(role);
  text = char(text);
  if isempty(text) || ~any(strcmp(role, {'user', 'assistant'}))
    return
  end
  turns = nelson.ai.internal.history.load(surfaceId);
  entry = struct('role', role, 'text', text);
  if isempty(turns)
    turns = entry;
  else
    turns(end + 1) = entry;
  end
  maxTurns = 100;
  if numel(turns) > maxTurns
    turns = turns(end - maxTurns + 1:end);
  end
  try
    d = historyDir();
    if ~isfolder(d)
      mkdir(d);
    end
    filewrite(historyFile(surfaceId), jsonencode(turns));
  catch
  end
end
%=============================================================================
