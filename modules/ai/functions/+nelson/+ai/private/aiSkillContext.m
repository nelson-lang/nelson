%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build a guidance preamble from the catalog skills that apply to a prompt, so
% the live agent receives the relevant how-to (for example the NFlow model
% authoring API) without depending on the gated MCP tool channel. It is
% backend-agnostic: the same preamble feeds the ACP agent and the local agent.
%
% surfaceId selects a surface-specific guide so it does NOT leak between the
% assistant surfaces that share the process:
%   - 'nflow' -> the embedded NFlow pane's dedicated build guide ONLY (it can
%     build models on its canvas), never the other surfaces.
%   - otherwise -> catalog skills whose triggers/name match the prompt, minus
%     the NFlow-pane guide (which references a canvas the other surfaces lack).
%
% Returns '' when nothing applies, so callers can prepend unconditionally.
function text = aiSkillContext(prompt, surfaceId)
  narginchk(1, 2);
  if nargin < 2
    surfaceId = '';
  end
  text = '';
  wanted = wantedSkillNames(char(prompt), char(surfaceId));
  if isempty(wanted)
    return
  end
  blocks = skillBlocks(wanted);
  if isempty(blocks)
    return
  end
  header = ['Project skill guide(s) for this request. Follow them exactly when ', ...
    'building, wiring, saving, or simulating NFlow models in the live session.'];
  text = [header, newline, newline, strjoin(blocks, [newline, newline])];
end
%=============================================================================
% Names of the skills that apply, selected by surface then by trigger match.
function names = wantedSkillNames(prompt, surfaceId)
  % The embedded NFlow pane gets its dedicated build guide, selected by surface
  % so it never leaks to the Qt dock or the web pane (which share the process and
  % have no canvas to build on).
  if strcmp(surfaceId, 'nflow')
    names = {'nflow-assistant'};
    return
  end
  try
    matched = nelson.ai.skills(prompt);
  catch
    matched = struct('name', {}, 'path', {});
  end
  names = {};
  if ~isempty(matched)
    names = {matched.name};
  end
  % The NFlow-pane guide is surface-specific; never inject it on other surfaces.
  names = names(~strcmp(names, 'nflow-assistant'));
  if ~isempty(names)
    names = unique(names, 'stable');
  end
end
%=============================================================================
% For each wanted name present in the catalog, a '<skill>...</skill>' block
% wrapping the SKILL.md body.
function blocks = skillBlocks(wanted)
  catalog = allSkills();
  blocks = cell(1, 0);
  if isempty(catalog)
    return
  end
  catalogNames = {catalog.name};
  for k = 1:numel(wanted)
    index = find(strcmp(catalogNames, wanted{k}), 1);
    if isempty(index)
      continue
    end
    body = skillBody(catalog(index).path);
    if isempty(body)
      continue
    end
    blocks{end + 1} = ['<skill name="', catalog(index).name, '">', newline, ...
      body, newline, '</skill>']; %#ok<AGROW>
  end
end
%=============================================================================
function catalog = allSkills()
  try
    catalog = nelson.ai.skills();
  catch
    catalog = struct('name', {}, 'path', {});
  end
end
%=============================================================================
% Return the Markdown body of a SKILL.md with its YAML front-matter removed.
function body = skillBody(skillPath)
  body = '';
  if ~isfile(skillPath)
    return
  end
  lines = strsplit(fileread(skillPath), newline);
  start = frontMatterEnd(lines);
  body = strtrim(strjoin(lines(start:end), newline));
end
%=============================================================================
% First line index after the leading '---' front-matter block (1 when absent).
function start = frontMatterEnd(lines)
  start = 1;
  if isempty(lines) || ~strcmp(strtrim(lines{1}), '---')
    return
  end
  for k = 2:numel(lines)
    if strcmp(strtrim(lines{k}), '---')
      start = k + 1;
      return
    end
  end
end
%=============================================================================
