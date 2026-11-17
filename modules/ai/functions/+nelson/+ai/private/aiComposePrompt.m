%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Assemble the full prompt sent to the model from the user's own text, layering
% the context the backends need. One place owns the order so every surface and
% both backends (ACP and HTTP) stay consistent. The pane still shows only the
% user's text; this augments only what the model receives.
%
% Order (each section omitted when empty):
%   1. Identity + ground rules (always on).
%   2. Verified Nelson API for functions named in the prompt (real signatures).
%   3. Closest shipped runnable example (a Nelson demo off the NFlow surface, a
%      .nflow model on it), as a real-idiom / correct-wiring pattern.
%   4. Project skill guide(s) that match the request (end-user audience only).
%   5. Host-action bridge (ACP only: lets the agent pilot the live GUI).
%   6. The user's request, last, clearly delimited.
function augmented = aiComposePrompt(userPrompt, surfaceId, isAcp)
  narginchk(2, 3);
  if nargin < 3
    isAcp = false;
  end
  userPrompt = char(userPrompt);
  surfaceId = char(surfaceId);
  sections = {};
  sections = appendSection(sections, aiAssistantIdentity(surfaceId));
  sections = appendSection(sections, aiApiGrounding(userPrompt));
  sections = appendSection(sections, aiNelsonDemoContext(userPrompt, surfaceId));
  sections = appendSection(sections, aiSkillContext(userPrompt, surfaceId));
  sections = appendSection(sections, aiNflowExampleContext(userPrompt, surfaceId));
  if isAcp
    sections = appendSection(sections, acpHostActionPreamble());
  end
  context = strjoin(sections, [char(10), char(10)]);
  augmented = [context, char(10), char(10), 'User request:', char(10), char(10), userPrompt];
end
%=============================================================================
function sections = appendSection(sections, text)
  if ~isempty(text)
    sections{end + 1} = text;
  end
end
%=============================================================================
