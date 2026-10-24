%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateExampleModule(moduleName)
  if ismodule(moduleName)
    moduleRoot = modulepath(moduleName);
  else
    moduleRoot = fullfile(nelsonroot(), 'modules', moduleName);
  end
  examplesRoot = fullfile(moduleRoot, 'examples');
  manifestPath = fullfile(examplesRoot, 'index.json');
  asserts.istrue(isfile(manifestPath));

  manifest = jsondecode(fileread(manifestPath));
  asserts.istrue(isfield(manifest, 'schemaVersion'));
  asserts.isequal(manifest.schemaVersion, 1);
  asserts.istrue(isfield(manifest, 'examples'));

  entries = manifest.examples;
  for index = 1:numel(entries)
    if iscell(entries)
      entry = entries{index};
    else
      entry = entries(index);
    end
    validateEntry(entry, examplesRoot);
  end
end

function validateEntry(entry, examplesRoot)
  hasFile = isfield(entry, 'file');
  hasCommand = isfield(entry, 'command');
  asserts.istrue(xor(hasFile, hasCommand));
  asserts.istrue(isfield(entry, 'title'));
  asserts.istrue(isfield(entry, 'description'));
  asserts.istrue(isfield(entry, 'tags'));
  asserts.istrue(isfield(entry, 'requirements'));
  validateText(entry.title);
  validateText(entry.description);

  if hasCommand
    validateCommandEntry(entry);
    return;
  end

  relativeFile = char(entry.file);
  asserts.isfalse(contains(relativeFile, '..'));
  asserts.isequal(fileparts(relativeFile, 'extension'), '.m');
  examplePath = fullfile(examplesRoot, relativeFile);
  asserts.istrue(isfile(examplePath));

  source = fileread(examplePath);
  loweredSource = lower(source);
  terminationPattern = '(^|[;,\r\n][ \t]*)(exit|quit)[ \t]*(\(|;|\r|\n|$)';
  asserts.istrue(isempty(regexp(loweredSource, terminationPattern, 'once')));

  requirements = entry.requirements;
  if isempty(requirements)
    runOutsideModule(examplePath);
  else
    validateRequirementNames(requirements);
    [~, baselineRequirements] = galleryRequirementCatalog();
    guardedRequirements = requirements(~ismember(requirements, baselineRequirements));
    if ~isempty(guardedRequirements)
      asserts.istrue(contains(loweredSource, 'error('));
    end
    validateNoArgumentFunction(source);
  end
end

function validateCommandEntry(entry)
  command = entry.command;
  asserts.istrue((ischar(command) || isstring(command)) && strlength(command) > 0);
  validateRequirementNames(entry.requirements);
end

function validateText(value)
  if isstruct(value)
    names = fieldnames(value);
    asserts.istrue(~isempty(names));
    asserts.istrue(all(ismember(names, {'en_US'; 'fr_FR'})));
    for index = 1:numel(names)
      text = value.(names{index});
      asserts.istrue((ischar(text) || isstring(text)) && strlength(text) > 0);
    end
  else
    asserts.istrue((ischar(value) || isstring(value)) && strlength(value) > 0);
  end
end

function validateRequirementNames(requirements)
  allowed = galleryRequirementCatalog();
  asserts.istrue(all(ismember(requirements, allowed)));
end

function [allowed, baseline] = galleryRequirementCatalog()
  % Reads the shared gallery requirement vocabulary from etc/gallery_requirements.json.
  % allowed  : every capability name a requirements field may use.
  % baseline : names always present in a standard build (no runtime error() guard needed).
  persistent cachedAllowed cachedBaseline
  if isempty(cachedAllowed)
    manifestPath = fullfile(nelsonroot(), 'etc', 'gallery_requirements.json');
    catalog = jsondecode(fileread(manifestPath));
    entries = catalog.requirements;
    cachedAllowed = cell(numel(entries), 1);
    cachedBaseline = {};
    for index = 1:numel(entries)
      cachedAllowed{index} = entries(index).name;
      if entries(index).baseline
        cachedBaseline{end + 1, 1} = entries(index).name; %#ok<AGROW>
      end
    end
  end
  allowed = cachedAllowed;
  baseline = cachedBaseline;
end

function validateNoArgumentFunction(source)
  lines = strsplit(source, newline());
  firstCodeLine = '';
  for index = 1:numel(lines)
    candidate = strtrim(lines{index});
    if ~isempty(candidate) && ~startsWith(candidate, '%')
      firstCodeLine = candidate;
      break;
    end
  end
  if startsWith(firstCodeLine, 'function')
    declaration = regexp(firstCodeLine, '\(([^)]*)\)', 'tokens', 'once');
    if ~isempty(declaration)
      asserts.istrue(isempty(strtrim(declaration{1})));
    end
  end
end

function runOutsideModule(examplePath)
  previousDirectory = pwd();
  executionRoot = tempname();
  mkdir(executionRoot);
  cleanup = onCleanup(@() restoreDirectory(previousDirectory, executionRoot));
  cd(executionRoot);
  run(examplePath);
  clear cleanup;
end

function restoreDirectory(previousDirectory, executionRoot)
  cd(previousDirectory);
  if isdir(executionRoot)
    rmdir(executionRoot, 's');
  end
end
