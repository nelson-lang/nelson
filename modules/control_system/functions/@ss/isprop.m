%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isprop(obj, propertyName)
  if ~ischar(propertyName)
    error(_('Wrong type for argument #2: string expected.'));
  end    
  tf = any(strcmp(properties(obj), propertyName));
end
%=============================================================================
