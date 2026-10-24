%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [payloadRoots, paths] = __nelsonc_stage_paths__(plan, payload)
  roots = plan.searchRoots;
  if isstring(roots)
    roots = cellstr(roots);
  end
  payloadRoots = {};
  for k = 1:numel(roots)
    payloadRoots{k} = ['roots/', num2str(k)];
  end
  directories = payloadRoots;
  if isfield(plan, 'directories')
    directories = [directories, mapped_paths(plan.directories, roots, payloadRoots)];
  end
  paths = {};
  if isfield(plan, 'pathEntries')
    paths = mapped_paths(plan.pathEntries, roots, payloadRoots);
  end
  for k = 1:numel(payloadRoots)
    if ~any(strcmp(paths, payloadRoots{k}))
      paths{end + 1} = payloadRoots{k};
    end
  end
  % Validate the complete plan before creating any directories or copying files.
  for k = 1:numel(plan.files)
    if isempty(plan.files(k).module)
      mapped_paths(struct('root', plan.files(k).root, 'relative', plan.files(k).relative), roots, payloadRoots);
      if isempty(plan.files(k).relative)
        error('Nelson:nelsonc:inputPath', 'Application file path must not be empty.');
      end
    end
  end
  for k = 1:numel(directories)
    mkdir(fullfile(payload, directories{k}));
  end
end

function paths = mapped_paths(records, roots, payloadRoots)
  paths = {};
  if isempty(records)
    return;
  end
  if ~isstruct(records) || ~isfield(records, 'root') || ~isfield(records, 'relative')
    error('Nelson:nelsonc:inputPath:invalidApplicationPathRecords', 'Invalid application path records.');
  end
  for k = 1:numel(records)
    record = records(k);
    relative = record.relative;
    if ~ischar(record.root) || ~isrow(record.root) || ~ischar(relative) ...
      || (~isempty(relative) && ~isrow(relative)) ...
      || any(relative == '\') || any(relative == ':') || any(relative == char(0))
      error('Nelson:nelsonc:inputPath:invalidApplicationPath', 'Invalid application path.');
    end
    index = find(strcmp(roots, record.root), 1);
    if isempty(index)
      error('Nelson:nelsonc:inputPath:unknownApplicationSearchRoot', 'Unknown application search root.');
    end
    destination = payloadRoots{index};
    if ~isempty(relative)
      components = strsplit(relative, '/', 'CollapseDelimiters', false);
      if any(strcmp(components, '..')) || any(strcmp(components, '.')) ...
        || any(cellfun('isempty', components))
        error('Nelson:nelsonc:inputPath:invalidRelativeApplicationPath', 'Invalid relative application path.');
      end
      destination = [destination, '/', relative];
    end
    if ~any(strcmp(paths, destination))
      paths{end + 1} = destination;
    end
  end
end
