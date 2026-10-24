%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function n = report_countSkipped(cases)
  n = 0;
  for k = 1:length(cases)
    n = n + report_isSkippedStatus(cases(k));
  end
end
