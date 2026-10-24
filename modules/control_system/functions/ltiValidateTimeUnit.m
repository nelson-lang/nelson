%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = ltiValidateTimeUnit(value)
  value = ltiValidateTextScalar('TimeUnit', value);
  supported = {'nanoseconds', 'microseconds', 'milliseconds', 'seconds', ...
    'minutes', 'hours', 'days', 'weeks', 'months', 'years'};
  if ~any(strcmp(value, supported))
    error('Nelson:control_system:invalidTimeUnit', '%s must be a valid time unit.', 'TimeUnit property');
  end
end
%=============================================================================
