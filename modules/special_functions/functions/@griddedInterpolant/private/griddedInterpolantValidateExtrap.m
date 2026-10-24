%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function extrap = griddedInterpolantValidateExtrap(value)
  allowed = {'linear', 'nearest', 'next', 'previous', 'pchip', 'cubic', 'spline', 'makima', 'none'};
  if ~griddedInterpolantIsText(value)
    error('Nelson:griddedInterpolant:invalidExtrapolationMethod', 'The extrapolation method must be ''linear'', ''nearest'', ''next'', ''previous'', ''pchip'', ''cubic'', ''spline'', ''makima'', or ''none''.');
  end
  extrap = convertStringsToChars(value);
  if ~any(strcmp(extrap, allowed))
    error('Nelson:griddedInterpolant:invalidExtrapolationMethod', 'The extrapolation method must be ''linear'', ''nearest'', ''next'', ''previous'', ''pchip'', ''cubic'', ''spline'', ''makima'', or ''none''.');
  end
end
%=============================================================================
