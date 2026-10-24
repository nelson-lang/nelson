%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = ltiCopyCellMetadata(value, count)
  if isempty(value)
    value = {};
    return
  end
  if ~iscell(value) || numel(value) ~= count
    value = {};
    return
  end
  value = value(:)';
end
%=============================================================================
