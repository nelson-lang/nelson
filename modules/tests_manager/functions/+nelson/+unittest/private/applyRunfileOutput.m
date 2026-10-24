%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_res = applyRunfileOutput(test_case_res, s)
  if isfield(s, 'stdout')
    test_case_res.stdout = s.stdout;
  end
end
