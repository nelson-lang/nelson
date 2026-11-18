%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Per-domain function inventory for the skills that match a prompt: the REAL list
% of callable names in this build for each domain module the matched skills point
% to (their front-matter "modules:" field), read live with what(<module>). This
% complements aiApiGrounding (which grounds only the functions the user NAMED):
% here the agent also sees the neighbouring functions of the domain it did not
% name, so it can reach a better-fitting one instead of inventing a signature.
%
% Source of truth is what(), so the list never drifts from the build. Only focused
% domain modules are declared on skills (not catch-all modules like
% elementary_functions), and the block is capped per module and overall so it stays
% compact. Returns '' when no matched skill declares a usable module.
function text = aiModuleInventoryContext(prompt, surfaceId)
  narginchk(1, 2);
  if nargin < 2
    surfaceId = '';
  end
  text = '';
  if ~canListModules()
    return
  end
  selected = aiSelectedUserSkills(char(prompt), char(surfaceId));
  if isempty(selected)
    return
  end
  modules = collectModules(selected);
  if isempty(modules)
    return
  end
  maxModules = 8;
  maxPerModule = 50;
  maxTotal = 120;
  lines = {};
  total = 0;
  for k = 1:min(numel(modules), maxModules)
    if total >= maxTotal
      break
    end
    names = moduleFunctionNames(modules{k});
    if isempty(names)
      continue
    end
    room = min(maxPerModule, maxTotal - total);
    [shown, extra] = takeNames(names, room);
    total = total + numel(shown);
    line = ['- ', modules{k}, ': ', strjoin(shown, ', ')];
    if extra > 0
      line = [line, ' (+', int2str(extra), ' more)']; %#ok<AGROW>
    end
    lines{end + 1} = line; %#ok<AGROW>
  end
  if isempty(lines)
    return
  end
  header = ['Available Nelson functions by domain for this request (real names ', ...
    'callable in this build; use help/doc for exact signatures, and prefer these ', ...
    'over inventing names):'];
  text = [header, char(10), strjoin(lines, char(10))];
end
%=============================================================================
% True only when a module-scoped what() is available in this build (older builds
% expose the no-argument form only); keeps the section a no-op there.
function tf = canListModules()
  tf = false;
  try
    tf = nargin('what') ~= 0;
  catch
  end
end
%=============================================================================
% Distinct module names declared by the selected skills, in first-seen order.
function modules = collectModules(selected)
  modules = {};
  for k = 1:numel(selected)
    list = selected(k).modules;
    if ~iscell(list)
      continue
    end
    for j = 1:numel(list)
      name = strtrim(char(list{j}));
      if ~isempty(name) && ~any(strcmp(modules, name))
        modules{end + 1} = name; %#ok<AGROW>
      end
    end
  end
end
%=============================================================================
% Sorted, de-duplicated builtin+macro names of a module, with private names
% dropped. '' on any failure (unknown module, missing builtin) so one bad entry
% never breaks the section.
function names = moduleFunctionNames(moduleName)
  names = {};
  try
    [builtins, macros] = what(moduleName);
  catch
    return
  end
  merged = [reshape(cellstr(builtins), 1, []), reshape(cellstr(macros), 1, [])];
  keep = {};
  for k = 1:numel(merged)
    name = merged{k};
    if isPublicFunctionName(name)
      keep{end + 1} = name; %#ok<AGROW>
    end
  end
  if isempty(keep)
    return
  end
  names = unique(keep);
end
%=============================================================================
% A plain, user-callable function name: a leading letter then letters/digits/_,
% nothing else. Drops class methods (@lti/applySeriesMetadata), package-qualified
% names (images.roi.Ellipse) and private names (_foo), which are noise in a domain
% palette.
function tf = isPublicFunctionName(name)
  tf = ~isempty(name) && ~isempty(regexp(name, '^[A-Za-z][A-Za-z0-9_]*$', 'once'));
end
%=============================================================================
% The first room names, plus how many were left out.
function [shown, extra] = takeNames(names, room)
  if numel(names) > room
    shown = names(1:room);
    extra = numel(names) - room;
  else
    shown = names;
    extra = 0;
  end
end
%=============================================================================
