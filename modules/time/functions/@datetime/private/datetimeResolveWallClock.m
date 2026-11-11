%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [serial, millis, fold] = datetimeResolveWallClock(serial, millis, zone, sourceOffset)
  % Wall-clock serial dates of zone made valid: a time in a DST gap moves after
  % the gap, an ambiguous time is the occurrence whose UTC offset is
  % sourceOffset (seconds) when it has one, the later occurrence otherwise.
  fold = [];
  if isempty(zone)
    return
  end
  if nargin > 3 && ~isempty(sourceOffset)
    dataPath = [modulepath('time'), '/resources/tzdata'];
    [later, ambiguous] = timezoneoffset(serial, zone, dataPath);
    later = reshape(later, size(serial));
    fold = reshape(ambiguous, size(serial)) & later ~= sourceOffset;
    if ~any(fold(:))
      fold = [];
    end
  end
  [serial, millis, fold] = datetimeConvertZone(serial, millis, zone, zone, fold);
end
%=============================================================================
