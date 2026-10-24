%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = timeseriesDotReference(obj, name)
  state = struct(obj);
  switch name
    case 'Data'
      value = state.Data_;
    case 'Time'
      value = state.Time_;
    case 'Quality'
      value = state.Quality_;
    case 'IsTimeFirst'
      value = state.IsTimeFirst_;
    case 'Length'
      value = length(state.Time_);
    otherwise
      if isfield(state, name)
        value = state.(name);
      else
        error('Nelson:time:unknownTimeseriesProperty', 'Unknown %s property.', 'timeseries');
      end
  end
end
%=============================================================================
