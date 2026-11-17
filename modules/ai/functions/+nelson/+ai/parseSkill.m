%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Parse a SKILL.md file: read its front-matter name, description, triggers, and
% audience. audience is 'user' (served to the live end-user assistant) or
% 'contributor' (Nelson-development guidance, served only through the MCP skill
% resources); it defaults to 'contributor' so a skill without the field is never
% injected into an end-user prompt by accident.
function meta = parseSkill(skillPath)
  meta = struct('name', '', 'description', '', 'triggers', {{}}, ...
    'audience', 'contributor', 'path', skillPath);
  if ~isfile(skillPath)
    return
  end
  lines = splitFrontMatter(fileread(skillPath));
  for k = 1:numel(lines)
    [key, value] = splitKeyValue(lines{k});
    switch lower(key)
      case 'name'
        meta.name = value;
      case 'description'
        meta.description = value;
      case 'triggers'
        meta.triggers = splitList(value);
      case 'audience'
        meta.audience = normalizeAudience(value);
    end
  end
  if isempty(meta.name)
    meta.name = parentFolderName(skillPath);
  end
end
%=============================================================================
function lines = splitFrontMatter(text)
  lines = {};
  allLines = strsplit(text, char(10));
  started = false;
  for k = 1:numel(allLines)
    trimmed = strtrim(allLines{k});
    if strcmp(trimmed, '---')
      if ~started
        started = true;
        continue
      else
        break
      end
    end
    if started
      lines{end + 1} = trimmed;
    end
  end
end
%=============================================================================
function [key, value] = splitKeyValue(line)
  key = '';
  value = '';
  position = strfind(line, ':');
  if isempty(position)
    return
  end
  key = strtrim(line(1:position(1) - 1));
  value = strtrim(line(position(1) + 1:end));
end
%=============================================================================
function items = splitList(value)
  items = {};
  parts = strsplit(value, ',');
  for k = 1:numel(parts)
    item = strtrim(parts{k});
    if ~isempty(item)
      items{end + 1} = item;
    end
  end
end
%=============================================================================
function name = parentFolderName(skillPath)
  folder = fileparts(skillPath);
  [~, name] = fileparts(folder);
end
%=============================================================================
% Only 'user' and 'contributor' are valid; anything else (or empty) falls back to
% 'contributor', the safe default that keeps a skill out of end-user prompts.
function audience = normalizeAudience(value)
  value = lower(strtrim(value));
  if strcmp(value, 'user')
    audience = 'user';
  else
    audience = 'contributor';
  end
end
%=============================================================================
