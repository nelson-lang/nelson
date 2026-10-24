%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function zone = datetimeNumericSourceZone(options)
  % Time zone in which numeric datetime inputs are read ('' is UTC):
  % posixtime and juliandate count UTC instants, epochtime counts from its
  % Epoch (its own time zone when it has one), other numbers are wall clocks.
  switch lower(options.ConvertFrom)
    case {'posixtime', 'juliandate'}
      zone = '';
    case 'epochtime'
      zone = options.Epoch.TimeZoneValue;
      if isempty(zone)
        zone = options.TimeZone;
      end
    otherwise
      zone = options.TimeZone;
  end
end
%=============================================================================
