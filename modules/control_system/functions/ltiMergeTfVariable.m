%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function variable = ltiMergeTfVariable(left, right, fallback)
  if nargin < 3
    fallback = '';
  end
  if strcmp(left.Variable, right.Variable)
    variable = right.Variable;
    return
  end
  if isstatic(left) && ~isstatic(right)
    variable = right.Variable;
    return
  end
  if isstatic(right) && ~isstatic(left)
    variable = left.Variable;
    return
  end
  if strcmp(left.Variable, 'z^-1') || strcmp(right.Variable, 'z^-1')
    variable = 'z^-1';
    return
  end
  switch fallback
    case 'left'
      variable = left.Variable;
    case 'right'
      variable = right.Variable;
    otherwise
      variable = '';
  end
end
%=============================================================================
