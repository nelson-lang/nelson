%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function method = griddedInterpolantValidateMethod(value)
  allowed = {'linear', 'nearest', 'next', 'previous', 'pchip', 'cubic', 'spline', 'makima'};
  if ~griddedInterpolantIsText(value)
    error('Nelson:griddedInterpolant:invalidInterpolationMethod', 'The interpolation method must be ''linear'', ''nearest'', ''next'', ''previous'', ''pchip'', ''cubic'', ''spline'', or ''makima''.');
  end
  method = convertStringsToChars(value);
  if ~any(strcmp(method, allowed))
    error('Nelson:griddedInterpolant:invalidInterpolationMethod', 'The interpolation method must be ''linear'', ''nearest'', ''next'', ''previous'', ''pchip'', ''cubic'', ''spline'', or ''makima''.');
  end
end
%=============================================================================
