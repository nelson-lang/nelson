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
%   2. Live session state (workspace / last error / open file), when any.
%   3. Recent conversation (the direct backend has no memory of its own; the ACP
%      agent keeps its own, so it is injected only to SEED a fresh ACP session via
%      seedHistory - continuing a talk from another surface / after a restart).
%   4. Verified Nelson API for functions named in the prompt (real signatures).
%   5. Per-domain function inventory (real what(<module>) names for the matched
%      domains, so the agent can reach a neighbour it did not name).
%   6. Closest shipped runnable example (a Nelson demo off the NFlow surface, a
%      .nflow model on it), as a real-idiom / correct-wiring pattern.
%   7. Project skill guide(s) that match the request (end-user audience only).
%   8. Physical (acausal) block catalog for an NFlow request off the NFlow pane
%      (the pane injects its own live catalog).
%   9. Host-action bridge (ACP only: lets the agent pilot the live GUI).
%  10. The user's request, last, clearly delimited.
%
% Sections 1-4 (identity, live state, history, verified API) and the user request
% are ALWAYS included. Sections 5-8 are enhancers: each is already capped, and they
% are added within a size budget so the prompt can never balloon as the catalog
% grows. When the budget is tight the highest-value enhancers (the matching skill
% guide, the closest runnable example) are kept and the reference lists (domain
% inventory, block catalog) are dropped first; whatever is kept is still emitted in
% the reading order above.
function augmented = aiComposePrompt(userPrompt, surfaceId, isAcp, seedHistory)
  narginchk(2, 4);
  if nargin < 3
    isAcp = false;
  end
  if nargin < 4
    seedHistory = false;
  end
  userPrompt = char(userPrompt);
  surfaceId = char(surfaceId);
  essentials = {};
  essentials = appendSection(essentials, aiAssistantIdentity(surfaceId));
  essentials = appendSection(essentials, aiLiveSessionContext(surfaceId));
  % The direct backend has no memory of its own, so always inject recent history.
  % The ACP agent keeps its own session memory, so inject ONLY to seed a FRESH ACP
  % session (seedHistory) - so a conversation continued on another surface or after a
  % restart has context - never on a session that already remembers its own turns.
  if ~isAcp || seedHistory
    essentials = appendSection(essentials, nelson.ai.internal.history.recent(surfaceId, 6));
  end
  essentials = appendSection(essentials, aiApiGrounding(userPrompt));
  enhancers = enhancerSections(userPrompt, surfaceId);
  sections = [essentials, selectWithinBudget(essentials, enhancers)];
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
% The non-empty enhancer sections, each with its reading order (section 5-8 above)
% and a priority (1 = keep first under pressure). Every context builder is called
% exactly once here.
function enh = enhancerSections(userPrompt, surfaceId)
  specs = { ...
    aiModuleInventoryContext(userPrompt, surfaceId), 4; ...
    aiNelsonDemoContext(userPrompt, surfaceId), 2; ...
    aiSkillContext(userPrompt, surfaceId), 1; ...
    aiNflowExampleContext(userPrompt, surfaceId), 3; ...
    aiNflowBlockCatalog(userPrompt, surfaceId), 5 };
  enh = struct('text', {}, 'priority', {}, 'order', {});
  for k = 1:size(specs, 1)
    if ~isempty(specs{k, 1})
      enh(end + 1) = struct('text', specs{k, 1}, 'priority', specs{k, 2}, 'order', k); %#ok<AGROW>
    end
  end
end
%=============================================================================
% Pick the enhancers that fit a total context budget, by priority, then return
% their texts in reading order. The essentials and the user request are never
% subject to the budget.
function texts = selectWithinBudget(essentials, enh)
  texts = {};
  if isempty(enh)
    return
  end
  budget = 16000;
  total = sectionsLength(essentials);
  [~, byPriority] = sort([enh.priority], 'ascend');
  keep = false(1, numel(enh));
  for i = byPriority
    addLen = length(enh(i).text) + 2;
    if total + addLen <= budget
      keep(i) = true;
      total = total + addLen;
    end
  end
  selected = enh(keep);
  if isempty(selected)
    return
  end
  [~, byOrder] = sort([selected.order], 'ascend');
  selected = selected(byOrder);
  texts = {selected.text};
end
%=============================================================================
% Total characters of a set of sections, counting the blank-line separator between
% them, as an approximation of the assembled length.
function n = sectionsLength(texts)
  n = 0;
  for k = 1:numel(texts)
    n = n + length(texts{k}) + 2;
  end
end
%=============================================================================
