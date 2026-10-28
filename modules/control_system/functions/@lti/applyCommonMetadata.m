%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = applyCommonMetadata(sys, left, right)
  [p, m] = size(left);
  sys.TimeUnit = mergeTimeUnit(left, right);
  sys.InputName = lti.mergeCellMetadata(left.InputName, m, right.InputName, m, 'match');
  sys.InputUnit = lti.mergeCellMetadata(left.InputUnit, m, right.InputUnit, m, 'match');
  sys.OutputName = lti.mergeCellMetadata(left.OutputName, p, right.OutputName, p, 'match');
  sys.OutputUnit = lti.mergeCellMetadata(left.OutputUnit, p, right.OutputUnit, p, 'match');
end
%=============================================================================
