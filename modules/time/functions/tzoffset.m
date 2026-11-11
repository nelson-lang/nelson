%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [d, dst] = tzoffset(t)
  narginchk(1, 1);
  nargoutchk(0, 2);
  if ~isa(t, 'datetime')
    error('Nelson:time:datetimeInputExpected', '%s expected.', 'Datetime input');
  end
  serials = datenum(t);
  % Unzoned datetimes and NaT have no offset.
  undefined = isnan(serials) | isempty(t.TimeZone);
  offsetSeconds = NaN(size(serials));
  dstSeconds = NaN(size(serials));
  if ~all(undefined(:))
    % first occurrences of ambiguous times (DST end) are read with 'first'
    first = datetime.foldOrFalse(t);
    [offsetSeconds, dstSeconds] = zoneOffsets(offsetSeconds, dstSeconds, serials, ...
      ~undefined & ~first, t.TimeZone, 'local', nargout > 1);
    if any(first(:))
      [offsetSeconds, dstSeconds] = zoneOffsets(offsetSeconds, dstSeconds, serials, ...
        ~undefined & first, t.TimeZone, 'first', nargout > 1);
    end
  end
  d = seconds(offsetSeconds);
  d.Format = 'hh:mm';
  if nargout > 1
    dst = seconds(dstSeconds);
    dst.Format = 'hh:mm';
  end
end
%=============================================================================
function [offsetSeconds, dstSeconds] = zoneOffsets(offsetSeconds, dstSeconds, serials, mask, zone, reference, withDst)
  % UTC offsets and daylight saving shifts of the masked wall-clock dates
  dataPath = [modulepath('time'), '/resources/tzdata'];
  offsetSeconds(mask) = timezoneoffset(serials(mask), zone, dataPath, reference);
  if withDst
    [~, dstSeconds(mask)] = timezonedst(serials(mask), zone, dataPath, reference);
  end
end
%=============================================================================
