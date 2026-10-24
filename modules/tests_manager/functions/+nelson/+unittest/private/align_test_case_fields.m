%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [left, right] = align_test_case_fields(left, right)
  if isempty(left) || isempty(right)
    return
  end
  leftNames = fieldnames(left);
  rightNames = fieldnames(right);
  for k = 1:length(rightNames)
    if ~isfield(left, rightNames{k})
      left = add_test_case_field(left, rightNames{k});
    end
  end
  for k = 1:length(leftNames)
    if ~isfield(right, leftNames{k})
      right = add_test_case_field(right, leftNames{k});
    end
  end
end
%=============================================================================
