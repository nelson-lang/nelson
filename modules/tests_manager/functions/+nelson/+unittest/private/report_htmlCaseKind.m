%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function kind = report_htmlCaseKind(testCase)
  kind = 'test';
  if isfield(testCase, 'kind')
    kind = testCase.kind;
  elseif isfield(testCase, 'isbench') && testCase.isbench
    kind = 'bench';
  end
end
%=============================================================================
