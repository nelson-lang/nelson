%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function files = discover_discoverDirectoryFiles(testsDir, kind)
  files = {};
  if ~isdir(testsDir)
    return
  end
  patterns = discover_patternsForKind(kind);
  for pattern = patterns
    entries = dir([testsDir, pattern{1}]);
    if ~isempty(entries)
      names = sort(string(strcat(testsDir, {entries.name})));
      files = [files; cell(names(:))];
    end
  end
end
