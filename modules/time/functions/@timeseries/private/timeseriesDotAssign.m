%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = timeseriesDotAssign(obj, name, value)
  switch name
    case 'Data'
      obj.Data = value;
    case 'Time'
      obj.Time = value;
    case 'Quality'
      obj.Quality = value;
    case 'IsTimeFirst'
      obj.IsTimeFirst = value;
    case 'Name'
      obj.Name = timeseriesText(value, 'Name');
    case {'Events', 'UserData', 'DataInfo', 'TimeInfo', 'QualityInfo', 'TreatNaNasMissing'}
      obj.(name) = value;
      if strcmp(name, 'TimeInfo')
        obj = timeseriesRefreshMetadata(obj);
      end
    otherwise
      error('Nelson:time:unknownTimeseriesProperty', 'Unknown %s property.', 'timeseries');
  end
end
%=============================================================================
