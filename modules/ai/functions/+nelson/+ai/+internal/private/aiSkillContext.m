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
  selected = aiSelectedUserSkills(char(prompt), char(surfaceId));
  if isempty(selected)
    return
  end
  blocks = skillBlocks(selected);
  if isempty(blocks)
    return
  end
  header = ['Project skill guide(s) for this request. Follow them exactly when ', ...
    'building, wiring, saving, or simulating NFlow models in the live session.'];
  text = [header, newline, newline, strjoin(blocks, [newline, newline])];
end
%=============================================================================
% For each selected skill, a '<skill>...</skill>' block wrapping the SKILL.md body.
function blocks = skillBlocks(selected)
  blocks = cell(1, 0);
  for k = 1:numel(selected)
    body = skillBody(selected(k).path);
    if isempty(body)
      continue
    end
    blocks{end + 1} = ['<skill name="', selected(k).name, '">', newline, ...
      body, newline, '</skill>']; %#ok<AGROW>
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
