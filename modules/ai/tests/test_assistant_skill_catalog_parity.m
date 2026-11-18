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
% registered in C++ (AiMcpSkillCatalog.cpp). The rule is by audience:
%   - every 'contributor' skill MUST be registered (dev agents reach it over MCP);
%   - 'user' skills are served to the assistant through aiSkillContext and need NOT
%     be registered, so new end-user skills add no C++ work;
%   - every registered resource (bar the catalog index) must have a folder.
% So a new contributor skill missing from the C++ catalog, or a stale registration,
% is caught here instead of shipping silently.
%=============================================================================
registered = nelson.ai.mcp.info().resources;
registered = registered(startsWith(registered, 'skills://'));
%=============================================================================
skills = nelson.ai.skills();
folderUris = {};
for k = 1:numel(skills)
  uri = uriFromPath(skills(k).path);
  assert_istrue(~isempty(uri));
  folderUris{end + 1} = uri; %#ok<AGROW>
  if strcmp(skills(k).audience, 'contributor')
    assert_istrue(any(strcmp(registered, uri)), ...
      ['Contributor skill ', uri, ' (', skills(k).name, ') is not registered in ', ...
      'AiMcpSkillCatalog.cpp.']);
  end
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
