%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = applySeriesMetadata(sys, left, right)
  [pLeft, ~] = size(left);
  [~, mRight] = size(right);
  sys.TimeUnit = mergeTimeUnit(left, right);
  sys.InputName = lti.copyCellMetadata(right.InputName, mRight);
  sys.InputUnit = lti.copyCellMetadata(right.InputUnit, mRight);
  sys.OutputName = lti.copyCellMetadata(left.OutputName, pLeft);
  sys.OutputUnit = lti.copyCellMetadata(left.OutputUnit, pLeft);
end
%=============================================================================
