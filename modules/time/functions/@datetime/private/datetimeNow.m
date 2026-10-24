%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function serial = datetimeNow(zone)
  % Current wall clock, in the given time zone when there is one.
  serial = now();
  if ~isempty(zone)
    localZone = timezonename('local', [modulepath('time'), '/resources/tzdata']);
    serial = datetimeConvertZone(serial, [], localZone, zone);
  end
end
%=============================================================================
