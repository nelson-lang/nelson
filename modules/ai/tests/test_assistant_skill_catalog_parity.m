%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Anti-drift guard between the SKILL.md files on disk and the MCP skill catalog
% registered in C++ (AiMcpSkillCatalog.cpp). A new skill folder must be either
% registered as an MCP resource or listed as an intentional exclusion, so adding a
% skill without wiring the C++ catalog is caught here instead of silently shipping
% a skill the MCP agents cannot see.
%=============================================================================
% Skills whose guide is deliberately NOT exposed as an MCP resource.
EXCLUDED = { ...
  'skills://nflow/assistant' ...  % live NFlow-pane-only guide, injected engine-side
  };
%=============================================================================
registered = nelson.ai.mcp.info().resources;
registered = registered(startsWith(registered, 'skills://'));
%=============================================================================
% Every SKILL.md on disk maps to an MCP uri; it must be registered or excluded.
skills = nelson.ai.skills();
folderUris = {};
for k = 1:numel(skills)
  uri = uriFromPath(skills(k).path);
  assert_istrue(~isempty(uri));
  folderUris{end + 1} = uri; %#ok<AGROW>
  isRegistered = any(strcmp(registered, uri));
  isExcluded = any(strcmp(EXCLUDED, uri));
  assert_istrue(isRegistered || isExcluded, ...
    ['Skill ', uri, ' (', skills(k).name, ') is neither registered in ', ...
    'AiMcpSkillCatalog.cpp nor listed as an intentional exclusion.']);
end
%=============================================================================
% Reverse: every registered skill resource (bar the catalog index) has a folder.
for k = 1:numel(registered)
  uri = registered{k};
  if strcmp(uri, 'skills://nelson/catalog')
    continue
  end
  assert_istrue(any(strcmp(folderUris, uri)), ...
    ['Registered MCP skill ', uri, ' has no SKILL.md folder under skills-catalog.']);
end
%=============================================================================
% 'skills://<top>/<sub>' from '.../skills-catalog/<top>/<sub>/SKILL.md'.
function uri = uriFromPath(skillPath)
  uri = '';
  parts = strsplit(strrep(char(skillPath), '\', '/'), '/');
  index = find(strcmp(parts, 'skills-catalog'), 1);
  if isempty(index) || index + 2 > numel(parts)
    return
  end
  uri = ['skills://', parts{index + 1}, '/', parts{index + 2}];
end
%=============================================================================
