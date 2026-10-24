%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function labels = polarNormalizeLabels(value)
  if ischar(value)
    labels = {value};
  elseif isstring(value)
    labels = cellstr(value(:));
  elseif iscellstr(value)
    labels = value(:);
  else
    error('Nelson:polarNormalizeLabels:invalidLabels', 'Labels must be strings.');
  end
end
%=============================================================================
