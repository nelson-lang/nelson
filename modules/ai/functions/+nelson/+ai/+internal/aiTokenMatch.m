%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Does a skill trigger token occur in a prompt? A single-word token must match a
% WHOLE word (so a short trigger like "c" for the C language matches "library C",
% not "calcule"); a multi-word token matches as a substring (phrase). Both sides
% are accent-folded so a French prompt matches unaccented triggers. Callers pass
% the folded prompt text and its word list (from aiPromptWords) once, so a scan of
% many tokens stays cheap.
function tf = aiTokenMatch(foldedPrompt, promptWords, token)
  narginchk(3, 3);
  token = nelson.ai.internal.aiFoldAccents(strtrim(token));
  if isempty(token)
    tf = false;
    return
  end
  if any(token == ' ')
    tf = contains(foldedPrompt, token);
  else
    tf = any(strcmp(promptWords, token));
  end
end
%=============================================================================
