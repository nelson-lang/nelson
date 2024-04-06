%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = single(obj)
  if ismethod(obj, 'single')
    r = obj.single();
  else
    error([_('Wrong value for #2 argument.'), ' ', 'single']);
  end
end
%=============================================================================
