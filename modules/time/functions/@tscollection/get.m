%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = get(obj, propertyName)
  if nargin == 1
    value = struct(obj);
    return
  end
  propertyName = timeseriesText(propertyName, 'Property name');
  if isfield(obj.Members_, propertyName)
    value = obj.Members_.(propertyName);
  else
    value = obj.(propertyName);
  end
end
%=============================================================================
