%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function name = report_caseName(testCase)
  if isfield(testCase, 'id')
    name = testCase.id;
  else
    name = testCase.name;
  end
end
