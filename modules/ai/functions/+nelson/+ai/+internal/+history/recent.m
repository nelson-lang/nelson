%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A compact transcript of the last maxTurns turns of a surface's conversation,
% formatted for injection into the prompt so a backend without its own memory
% (the direct HTTP/Ollama agent) can answer follow-up questions in context.
% Returns '' when there is no history. Each turn's text is trimmed to keep the
% injected block small.
function block = recent(surfaceId, maxTurns)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin < 2 || isempty(maxTurns)
    maxTurns = 6;
  end
  maxTurns = double(maxTurns);
  block = '';
  turns = nelson.ai.internal.history.load(surfaceId);
  if isempty(turns)
    return
  end
  n = numel(turns);
  startIdx = max(1, n - maxTurns + 1);
  lines = {};
  for k = startIdx:n
    if strcmp(turns(k).role, 'assistant')
      who = 'Assistant';
    else
      who = 'User';
    end
    lines{end + 1} = [who, ': ', clip(char(turns(k).text))]; %#ok<AGROW>
  end
  block = ['Earlier in this conversation (most recent last; for context only):', ...
    char(10), strjoin(lines, char(10))];
end
%=============================================================================
function t = clip(t)
  maxLen = 1200;
  if numel(t) > maxLen
    t = [t(1:maxLen), ' ...'];
  end
end
%=============================================================================
