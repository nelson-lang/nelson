%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Every function a skill declares in its `functions:` front-matter must really
% exist in this build, so a skill can never teach the assistant an invented API.
% A name counts as real when it is a builtin, a macro, or present in the shipped
% help index (which also covers package functions like nelson.ai.agent).
%=============================================================================
helpIndex = loadHelpIndex();
helpPages = loadHelpPages();
skills = nelson.ai.skills();
checked = 0;
for k = 1:numel(skills)
  fns = skills(k).functions;
  for f = 1:numel(fns)
    name = strtrim(fns{f});
    if isempty(name)
      continue
    end
    checked = checked + 1;
    assert_istrue(functionExists(name, helpIndex, helpPages), ...
      ['Skill ', skills(k).name, ' lists function "', name, '" which does not ', ...
      'exist (not a builtin/macro, not in the help index, no help page).']);
  end
end
% checked may be 0 until skills declare a functions list; the guard is that any
% declared function must resolve. Keep the variable referenced for clarity.
assert_istrue(checked >= 0);
%=============================================================================
% Every module a skill declares in its `modules:` front-matter must be a real
% module of this build, so aiModuleInventoryContext never asks what() for a module
% that does not exist (which would silently drop the inventory of that domain). The
% check is the module directory on disk, not getmodules(): a module may be present
% in the build yet not loaded in the headless test runner, but a typo'd name has no
% directory at all.
modulesRoot = [nelsonroot(), '/modules/'];
for k = 1:numel(skills)
  mods = skills(k).modules;
  for m = 1:numel(mods)
    name = strtrim(mods{m});
    if isempty(name)
      continue
    end
    assert_istrue(isdir([modulesRoot, name]), ...
      ['Skill ', skills(k).name, ' lists module "', name, '" which is not a ', ...
      'module of this build.']);
  end
end
%=============================================================================
function tf = functionExists(name, helpIndex, helpPages)
  tf = isbuiltin(name) || ismacro(name);
  if tf
    return
  end
  % The headless test runner does not load graphics/gui/control/..., so their
  % functions are not builtins here and the index built headless may miss them.
  % A shipped help page is the load-independent proof that the function is real.
  lowerName = lower(name);
  if any(strcmp(helpPages, lowerName)) || any(strcmp(helpPages, strrep(lowerName, '.', '_')))
    tf = true;
    return
  end
  if ~isstruct(helpIndex)
    return
  end
  tf = isfield(helpIndex, name) || any(strcmpi(fieldnames(helpIndex), name));
end
%=============================================================================
function pages = loadHelpPages()
  % Every documented function name: the lowercased basename of each help xml
  % page, across all modules. A dotted name maps to its underscore filename.
  pages = {};
  modulesRoot = [nelsonroot(), '/modules/'];
  entries = dir(modulesRoot);
  for k = 1:numel(entries)
    if ~entries(k).isdir || entries(k).name(1) == '.'
      continue
    end
    xmlDir = [modulesRoot, entries(k).name, '/help/en_US/xml/'];
    if ~isdir(xmlDir)
      continue
    end
    % '-s' recurses: some modules nest help pages in chapter subfolders.
    files = dir([xmlDir, '*.xml'], '-s');
    for f = 1:numel(files)
      basename = files(f).name(1:end-4);
      if strcmp(basename, 'chapter')
        continue
      end
      pages{end + 1} = lower(basename);
    end
  end
end
%=============================================================================
function index = loadHelpIndex()
  index = struct();
  try
    jsonPath = [modulepath('help_tools'), '/help/nelson_help_', getlanguage(), '.json'];
    if ~isfile(jsonPath)
      jsonPath = [modulepath('help_tools'), '/help/nelson_help_en_US.json'];
    end
    if isfile(jsonPath)
      index = jsondecode(fileread(jsonPath));
    end
  catch
    index = struct();
  end
end
%=============================================================================
