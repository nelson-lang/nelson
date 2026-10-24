%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = has_failed_cases(test_cases)
  tf = false;
  for k = 1:length(test_cases)
    tf = tf || strcmp(test_cases(k).status, 'Fail');
  end
end
%=============================================================================
