%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Prepare a prompt for trigger matching once: the accent-folded, lower-cased text
% (for phrase/substring triggers) and its list of words (for whole-word triggers).
% Pass both to aiTokenMatch so a scan of many tokens does the fold/split only once.
function [foldedPrompt, words] = aiPromptWords(prompt)
  narginchk(1, 1);
  foldedPrompt = nelson.ai.internal.aiFoldAccents(prompt);
  words = regexp(foldedPrompt, '[a-z0-9]+', 'match');
end
%=============================================================================
