%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = uint8(obj)
  if ismethod(obj, 'uint8')
    r = obj.uint8();
  else
    error([_('Wrong value for #2 argument.'), ' ', 'uint8']);
  end
end
%=============================================================================
