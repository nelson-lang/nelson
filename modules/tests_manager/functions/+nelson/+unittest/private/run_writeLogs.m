%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function results = run_writeLogs(results, logDir)
  if isempty(logDir)
    return
  end
  if ~isdir(logDir)
    mkdir(logDir);
  end
  cases = results.cases;
  for k = 1:length(cases)
    logFile = [logDir, '/', cases(k).id, '.json'];
    cases(k).artifacts = {logFile};
    filewrite(logFile, jsonprettyprint(jsonencode(cases(k))));
  end
  results.cases = cases;
end
