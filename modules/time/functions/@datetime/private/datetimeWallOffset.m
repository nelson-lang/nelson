%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function offset = datetimeWallOffset(serial, zone, fold)
  % UTC offsets (seconds) of wall-clock serial dates of zone; fold marks the
  % first occurrence of an ambiguous time ([] when there is none).
  dataPath = [modulepath('time'), '/resources/tzdata'];
  offset = reshape(timezoneoffset(serial, zone, dataPath), size(serial));
  if ~isempty(fold) && any(fold(:))
    offset(fold) = timezoneoffset(serial(fold), zone, dataPath, 'first');
  end
end
%=============================================================================
