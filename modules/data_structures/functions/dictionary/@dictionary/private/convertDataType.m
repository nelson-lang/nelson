%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ret = convertDataType(type, data)
  if isa(data, type)
    return;
  end
  if strcmp(type, 'cell') || strcmp(type, 'struct')
     error (_('Invalid conversion.'));
  end
  if strcmp(type, 'cell') && isnumeric(data)
    ret = num2str(data);
    return
  end
  ret = feval(type, data);
end
%=============================================================================
