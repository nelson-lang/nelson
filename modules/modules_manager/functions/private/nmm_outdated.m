%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = nmm_outdated(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if length(args) > 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  modules = nmm_list();
  names = fieldnames(modules);
  if length(args) == 1
    moduleName = args{1};
    if ~ischar(moduleName)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
    end
    names = {moduleName};
  end
  result = struct([]);
  for name = names(:)'
    moduleName = name{1};
    if ~isfield(modules, moduleName)
      continue
    end
    current = getfield(modules, moduleName);
    try
      latest = nmm_registry('info', moduleName);
      if semver(latest.version, current.version) == 1
        item.module = moduleName;
        item.current = current.version;
        item.latest = latest.version;
        result = appendItem(result, item);
      end
    catch
    end
  end
  result = nmm_json_output(result, jsonOutput);
end
%=============================================================================
function result = appendItem(result, item)
  if isempty(result)
    result = item;
  else
    result(end + 1) = item;
  end
end
%=============================================================================
