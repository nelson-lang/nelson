%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function method = interp_method(value, allowPchip, allowPreviousNext)
  if ~interp_is_text(value)
    error(_('Interpolation method expected.'));
  end
  method = convertStringsToChars(value);
  if strcmp(method, 'v5cubic')
    method = 'cubic';
  end
  allowed = {'linear', 'nearest', 'cubic', 'makima', 'spline'};
  if allowPchip
    allowed{end + 1} = 'pchip';
  end
  if allowPreviousNext
    allowed{end + 1} = 'previous';
    allowed{end + 1} = 'next';
  end
  if ~any(strcmp(method, allowed))
    error(_('Unknown interpolation method.'));
  end
end
%=============================================================================
