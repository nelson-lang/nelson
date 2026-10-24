%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = timeseriesBinaryOperation(a, b, op)
  if istimeseries(a) && istimeseries(b)
    if ~isequal(a.Time, b.Time)
      [a, b] = synchronize(a, b);
    end
    out = a;
    left = a.Data;
    right = b.Data;
  elseif istimeseries(a)
    out = a;
    left = a.Data;
    right = b;
  elseif istimeseries(b)
    out = b;
    left = a;
    right = b.Data;
  else
    error('Nelson:time:leastOneInputTimeseriesObject', 'At least one input must be a timeseries object.');
  end
  switch op
    case 'plus'
      out.Data_ = left + right;
    case 'minus'
      out.Data_ = left - right;
    case 'times'
      out.Data_ = left .* right;
    case 'rdivide'
      out.Data_ = left ./ right;
    case 'ldivide'
      out.Data_ = left .\ right;
    case 'mtimes'
      out.Data_ = left * right;
    case 'mrdivide'
      out.Data_ = left / right;
    case 'mldivide'
      out.Data_ = left \ right;
    case 'eq'
      out.Data_ = left == right;
    otherwise
      error('Nelson:time:unsupportedTimeseriesOperation', 'Unsupported %s.', 'timeseries operation');
  end
  out.Quality_ = [];
end
%=============================================================================
