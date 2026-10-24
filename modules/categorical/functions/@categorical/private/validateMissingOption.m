%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function option = validateMissingOption(option)
  if nargin == 0 || isempty(option)
    option = 'auto';
    return
  end
  if isstring(option)
    option = char(option);
  end
  if ~ischar(option)
    error('Nelson:categorical:missingOptionMustBeText', '%s must be text.', 'Missing option');
  end
end
%=============================================================================
