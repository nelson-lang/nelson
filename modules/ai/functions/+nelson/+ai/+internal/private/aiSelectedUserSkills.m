%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The catalog skills that apply to a prompt on a surface, as full meta structs,
% already filtered / ranked / capped. One selection feeds BOTH the skill guidance
% (aiSkillContext) and the per-domain function inventory (aiModuleInventoryContext),
% so the two never drift apart: every domain whose how-to is injected also gets its
% function inventory, and no other.
%
% Selection mirrors aiSkillContext:
%   - 'nflow' surface -> the embedded pane's dedicated build guide ONLY.
%   - otherwise -> end-user skills whose triggers match, minus the NFlow-pane guide,
%     ranked by match strength and capped so a growing catalog never floods a prompt.
% Returns a 0x0 struct array (with the catalog fields) when nothing applies.
function selected = aiSelectedUserSkills(prompt, surfaceId)
  narginchk(1, 2);
  if nargin < 2
    surfaceId = '';
  end
  prompt = char(prompt);
  surfaceId = char(surfaceId);
  catalog = allSkills();
  if strcmp(surfaceId, 'nflow')
    selected = pickByName(catalog, 'nflow-assistant');
    return
  end
  selected = catalog([]);
  if isempty(catalog)
    return
  end
  try
    matched = nelson.ai.skills(prompt);
  catch
    return
  end
  if isempty(matched)
    return
  end
  % Only end-user skills reach the live assistant prompt. Contributor skills stay
  % out (served to dev agents through MCP). The NFlow-pane guide is surface-specific.
  matched = matched(strcmp({matched.audience}, 'user'));
  matched = matched(~strcmp({matched.name}, 'nflow-assistant'));
  if isempty(matched)
    return
  end
  scores = zeros(1, numel(matched));
  for k = 1:numel(matched)
    scores(k) = scoreSkill(matched(k), prompt);
  end
  [~, order] = sort(scores, 'descend');
  matched = matched(order);
  maxSkills = 3;
  if numel(matched) > maxSkills
    matched = matched(1:maxSkills);
  end
  selected = matched;
end
%=============================================================================
function catalog = allSkills()
  try
    catalog = nelson.ai.skills();
  catch
    catalog = struct('name', {}, 'description', {}, 'triggers', {}, ...
      'audience', {}, 'functions', {}, 'modules', {}, 'path', {}, 'source', {});
  end
end
%=============================================================================
% The single catalog entry with this name (as a 1x1 struct array), or the empty
% catalog shape when it is absent.
function picked = pickByName(catalog, name)
  picked = catalog([]);
  if isempty(catalog)
    return
  end
  index = find(strcmp({catalog.name}, name), 1);
  if ~isempty(index)
    picked = catalog(index);
  end
end
%=============================================================================
% How many of a skill's signal tokens (curated triggers plus specific name words)
% appear in the prompt. Used only to rank matched skills; the stop words mirror
% nelson.ai.skills so ranking stays consistent with trigger matching.
function score = scoreSkill(skill, prompt)
  [foldedPrompt, promptWords] = nelson.ai.internal.aiPromptWords(prompt);
  stop = {'nelson', 'core', 'data', 'companion', 'work', 'tool', 'tools', ...
    'model', 'code', 'native', 'basics', 'workflows', 'assistant', 'usage'};
  tokens = skill.triggers;
  nameWords = strsplit(lower(skill.name), {'-', '_', '.', ' '});
  for w = 1:numel(nameWords)
    if length(nameWords{w}) >= 4 && ~any(strcmp(nameWords{w}, stop))
      tokens{end + 1} = nameWords{w}; %#ok<AGROW>
    end
  end
  score = 0;
  for t = 1:numel(tokens)
    if nelson.ai.internal.aiTokenMatch(foldedPrompt, promptWords, tokens{t})
      score = score + 1;
    end
  end
end
%=============================================================================
