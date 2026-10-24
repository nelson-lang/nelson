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
    error('Nelson:control_system:wrongType', 'Wrong type for argument #%d: %s expected.', 2, 'string');
  end
  tf = any(strcmp(ltiPropertyNames(class(obj)), propertyName));
end
%=============================================================================
