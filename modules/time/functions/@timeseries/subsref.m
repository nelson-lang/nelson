%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = subsref(obj, s)
  if strcmp(s(1).type, '()')
    value = timeseriesSubset(obj, s(1).subs{1});
    if length(s) > 1
      value = subsref(value, s(2:end));
    end
    return
  end
  if strcmp(s(1).type, '.')
    value = timeseriesDotReference(obj, s(1).subs);
    if length(s) > 1
      value = subsref(value, s(2:end));
    end
    return
  end
  error('Nelson:time:invalidTimeseriesIndexing', 'Invalid %s.', 'timeseries indexing');
end
%=============================================================================
