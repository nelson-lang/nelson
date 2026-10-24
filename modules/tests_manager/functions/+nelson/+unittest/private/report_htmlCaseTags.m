%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tags = report_htmlCaseTags(testCase)
  tags = '';
  if isfield(testCase, 'tags') && ~isempty(testCase.tags)
    if iscell(testCase.tags)
      tags = strjoin(testCase.tags, ', ');
    else
      tags = char(testCase.tags);
    end
  end
end
%=============================================================================
