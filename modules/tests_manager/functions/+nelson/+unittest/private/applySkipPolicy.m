%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = applySkipPolicy(test_case)
  test_case = applyResourceSkipPolicy(test_case);
  test_case = applyDisplaySkipPolicy(test_case);
  test_case = applyManualSkipPolicy(test_case);
  test_case = applyLanguageEngineSkipPolicy(test_case);
end
