%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function files = discover_discoverFiles(targets, kind) %#ok<NLS0026>
  files = {};
  if isempty(targets)
    targets = getmodules();
  end
  if ischar(targets)
    targets = {targets};
  end
  for item = targets(:)'
    target = item{1};
    if isfile(target)
      files = discover_appendIfKind(files, target, kind);
    elseif ismodule(target)
      files = [files; discover_discoverModuleFiles(target, kind)];
    elseif isdir(target)
      files = [files; discover_discoverDirectoryFiles(target, kind)];
    end
  end
end
