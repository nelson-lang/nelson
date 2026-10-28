%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = mergeCellMetadata(left, leftCount, right, rightCount, mode)
  switch mode
    case 'concat'
      value = concatMetadata(left, leftCount, right, rightCount);
    case 'match'
      value = matchMetadata(left, leftCount, right, rightCount);
    otherwise
      error('Nelson:control_system:invalidSyntax', 'Invalid syntax.');
  end
end
%=============================================================================
function value = concatMetadata(left, leftCount, right, rightCount)
  if isempty(left) && isempty(right)
    value = {};
    return
  end
  left = expandMetadata(left, leftCount);
  right = expandMetadata(right, rightCount);
  if isempty(left) || isempty(right)
    value = {};
    return
  end
  value = [left, right];
end
%=============================================================================
function value = matchMetadata(left, leftCount, right, rightCount)
  if leftCount ~= rightCount || isempty(left) || isempty(right)
    value = {};
    return
  end
  left = expandMetadata(left, leftCount);
  right = expandMetadata(right, rightCount);
  if isequal(left, right)
    value = left;
  else
    value = {};
  end
end
%=============================================================================
function value = expandMetadata(value, count)
  if isempty(value)
    value = cell(1, count);
    value(:) = {''};
    return
  end
  if ~iscell(value) || numel(value) ~= count
    value = {};
    return
  end
  value = value(:)';
end
%=============================================================================
