%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [directories, childRpaths] = __nelsonc_elf_search_paths__(info, source, inherited)
  origin = fileparts(source);
  rpaths = {};
  if isempty(info.runpaths)
    rpaths = expand_paths(info.rpaths, origin);
  end
  childRpaths = [rpaths(:); inherited(:)];
  environment = getenv('LD_LIBRARY_PATH');
  environmentPaths = {};
  if ~isempty(environment)
    arguments = argv();
    executableOrigin = fileparts(fullpath(arguments{1}));
    environmentPaths = expand_paths({strrep(environment, ';', ':')}, executableOrigin);
  end
  if isempty(info.runpaths)
    directories = [childRpaths; environmentPaths(:)];
  else
    % RUNPATH applies only to direct dependencies and suppresses this object's RPATH.
    directories = [environmentPaths(:); expand_paths(info.runpaths, origin)];
  end
  if numel(directories) > 4096 || numel(childRpaths) > 4096
    error('Nelson:nelsonc:nativeLimit', 'Native library search path limit exceeded.');
  end
end

function directories = expand_paths(values, origin)
  directories = {};
  for k = 1:numel(values)
    parts = strsplit(values{k}, ':', 'CollapseDelimiters', false);
    if numel(parts) > 4096 - numel(directories)
      error('Nelson:nelsonc:nativeLimit', 'Native library search path limit exceeded.');
    end
    for j = 1:numel(parts)
      value = __nelsonc_elf_expand_origin__(parts{j}, origin);
      if isempty(value)
        value = pwd();
      end
      directories{end + 1, 1} = fullpath(value);
    end
  end
end
