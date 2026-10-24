%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function status = report_writeJson(results, outputFile)
  if isempty(outputFile)
    error('Nelson:tests_manager:outputFilenameExpected', 'Output filename expected.');
  end
  filewrite(outputFile, jsonprettyprint(jsonencode(results)));
  status = isfile(outputFile);
end
