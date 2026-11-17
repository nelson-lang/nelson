%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% List assistant skills from the built-in catalog and the user preferences
% directory, merged by name (a user skill overrides a built-in one). With a
% text argument, only skills whose triggers match the text are returned.
function result = skills(query)
  narginchk(0, 1);
  result = struct('name', {}, 'description', {}, 'triggers', {}, 'audience', {}, ...
    'path', {}, 'source', {});
  names = {};
  sources = skillSources();
  for s = 1:numel(sources)
    files = findSkillFiles(sources(s).dir);
    for f = 1:numel(files)
      meta = nelson.ai.parseSkill(files{f});
      if isempty(meta.name)
        continue
      end
      meta.source = sources(s).source;
      index = find(strcmp(names, meta.name), 1);
      if isempty(index)
        names{end + 1} = meta.name;
        result(end + 1) = meta;
      else
        result(index) = meta;
      end
    end
  end
  if nargin == 1 && ~isempty(result)
    result = filterByTrigger(result, char(query));
  end
end
%=============================================================================
function sources = skillSources()
  % Precedence is applied by the merge order: a later source overrides an
  % earlier one of the same name, so builtin < module < user.
  sources = struct('dir', {}, 'source', {});
  builtinDir = [modulepath('ai', 'root'), '/skills-catalog'];
  if isfolder(builtinDir)
    sources(end + 1) = struct('dir', builtinDir, 'source', 'builtin');
  end
  moduleNames = getmodules();
  for m = 1:numel(moduleNames)
    moduleDir = [modulepath(moduleNames{m}, 'root'), '/skills'];
    if isfolder(moduleDir)
      sources(end + 1) = struct('dir', moduleDir, 'source', 'module');
    end
  end
  userDir = [prefdir(), '/assistant/skills'];
  if isfolder(userDir)
    sources(end + 1) = struct('dir', userDir, 'source', 'user');
  end
end
%=============================================================================
function files = findSkillFiles(root)
  files = {};
  entries = dir(root);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..')
      continue
    end
    full = [root, '/', name];
    if isfolder(full)
      files = [files, findSkillFiles(full)];
    elseif strcmpi(name, 'SKILL.md')
      files{end + 1} = full;
    end
  end
end
%=============================================================================
function matched = filterByTrigger(allSkills, query)
  query = lower(query);
  keep = false(1, numel(allSkills));
  for k = 1:numel(allSkills)
    tokens = skillTokens(allSkills(k));
    for t = 1:numel(tokens)
      token = lower(tokens{t});
      if ~isempty(token) && contains(query, token)
        keep(k) = true;
        break
      end
    end
  end
  matched = allSkills(keep);
end
%=============================================================================
% Match tokens for a skill: the curated triggers, plus name words that are
% SPECIFIC enough to be a real intent signal. Generic catalog-structure words
% (mostly the family prefixes such as 'nelson' and 'core') are dropped: left in,
% 'nelson' alone matched any prompt mentioning Nelson and flooded every skill.
% Curated triggers remain the primary, reliable signal.
function tokens = skillTokens(skill)
  tokens = skill.triggers;
  stop = {'nelson', 'core', 'data', 'companion', 'work', 'tool', 'tools', ...
    'model', 'code', 'native', 'basics', 'workflows', 'assistant'};
  nameWords = strsplit(lower(skill.name), {'-', '_', '.', ' '});
  for w = 1:numel(nameWords)
    word = nameWords{w};
    if length(word) >= 4 && ~any(strcmp(word, stop))
      tokens{end + 1} = word;
    end
  end
end
%=============================================================================
