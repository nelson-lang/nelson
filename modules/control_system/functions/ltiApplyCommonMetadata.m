%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = ltiApplyCommonMetadata(sys, left, right)
  [p, m] = size(left);
  sys.TimeUnit = ltiMergeTimeUnit(left, right);
  sys.InputName = ltiMergeCellMetadata(left.InputName, m, right.InputName, m, 'match');
  sys.InputUnit = ltiMergeCellMetadata(left.InputUnit, m, right.InputUnit, m, 'match');
  sys.OutputName = ltiMergeCellMetadata(left.OutputName, p, right.OutputName, p, 'match');
  sys.OutputUnit = ltiMergeCellMetadata(left.OutputUnit, p, right.OutputUnit, p, 'match');
end
%=============================================================================
