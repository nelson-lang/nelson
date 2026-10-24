%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = delsample(obj, varargin)
  if nargin == 2
    rows = timeseriesResolveRows(obj, varargin{1});
  elseif nargin == 3 && strcmpi(timeseriesText(varargin{1}, 'Selector'), 'index')
    rows = timeseriesResolveRows(obj, varargin{2});
  elseif nargin == 3 && strcmpi(timeseriesText(varargin{1}, 'Selector'), 'time')
    rows = find(obj.Time == varargin{2});
  else
    error('Nelson:time:invalidSampleSelector', 'Invalid %s.', 'sample selector');
  end
  keep = true(obj.Length, 1);
  keep(rows) = false;
  obj = timeseriesSubset(obj, find(keep));
end
%=============================================================================
