%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = ltiApplySeriesMetadata(sys, left, right)
  [pLeft, ~] = size(left);
  [~, mRight] = size(right);
  sys.TimeUnit = ltiMergeTimeUnit(left, right);
  sys.InputName = ltiCopyCellMetadata(right.InputName, mRight);
  sys.InputUnit = ltiCopyCellMetadata(right.InputUnit, mRight);
  sys.OutputName = ltiCopyCellMetadata(left.OutputName, pLeft);
  sys.OutputUnit = ltiCopyCellMetadata(left.OutputUnit, pLeft);
end
%=============================================================================
