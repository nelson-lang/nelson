%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = genpath(d)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ~(ischar(d) || (isstring(d) && isscalar(d)))
    error('Nelson:files_folders_functions:inputCharacterVectorStringScalar', 'Input must be a character vector or string scalar.');
  end
  d = char(d);
  if isempty(d) || ~isfolder(d)
    p = '';
    return
  end
  p = genpathRecursive(fullpath(d));
end
%=============================================================================
function p = genpathRecursive(d)
  p = [d, pathsep()];
  entries = dir(d);
  for k = 1:length(entries)
    name = entries(k).name;
    if entries(k).isdir && shouldInclude(name)
      p = [p, genpathRecursive(fullfile(d, name))];
    end
  end
end
%=============================================================================
function tf = shouldInclude(name)
  tf = ~(strcmp(name, '.') || strcmp(name, '..') || strcmp(name, 'private') ...
    || strcmp(name, 'resources') || startsWith(name, '@') || startsWith(name, '+'));
end
%=============================================================================
