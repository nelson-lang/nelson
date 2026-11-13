%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function seconds = posixSeconds(when)
  % POSIX time of a datetime scalar (local time when it has no time zone).
  if ~isa(when, 'datetime') || ~isscalar(when)
    error('Nelson:io:invalidOption', 'Modified must be a datetime scalar.');
  end
  localTime = when;
  if isempty(localTime.TimeZone)
    localTime.TimeZone = 'local';
  end
  seconds = posixtime(localTime);
end
%=============================================================================
