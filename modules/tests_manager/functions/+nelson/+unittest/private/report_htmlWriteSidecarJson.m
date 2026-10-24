%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function report_htmlWriteSidecarJson(results, outputFile)
  sidecarFile = report_htmlSidecarFilename(outputFile);
  if ~isempty(sidecarFile)
    filewrite(sidecarFile, jsonprettyprint(jsonencode(results)));
  end
end
%=============================================================================
function sidecarFile = report_htmlSidecarFilename(outputFile)
  sidecarFile = '';
  dotIndex = strfind(outputFile, '.');
  if isempty(dotIndex)
    sidecarFile = [outputFile, '.json'];
  else
    sidecarFile = [outputFile(1:dotIndex(end) - 1), '.json'];
  end
end
%=============================================================================
