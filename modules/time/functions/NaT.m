%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = NaT(varargin)
  [sizes, fmt, tz] = parseNaTInputs(varargin{:});
  if isempty(sizes)
    values = NaN;
  elseif numel(sizes) == 1
    values = NaN(sizes{1}, sizes{1});
  else
    values = NaN(sizes{:});
  end
  t = datetime(values, 'ConvertFrom', 'datenum', 'Format', fmt, 'TimeZone', tz);
end
%=============================================================================
function [sizes, fmt, tz] = parseNaTInputs(varargin)
  sizes = {};
  fmt = 'dd-MMM-yyyy';
  tz = '';
  k = 1;
  while k <= nargin
    if ischar(varargin{k}) || (isstring(varargin{k}) && isscalar(varargin{k}))
      name = char(varargin{k});
      if strcmpi(name, 'Format')
        fmt = char(varargin{k + 1});
        k = k + 2;
        continue
      elseif strcmpi(name, 'TimeZone')
        tz = char(varargin{k + 1});
        k = k + 2;
        continue
      end
    end
    value = varargin{k};
    if isnumeric(value) && isvector(value) && numel(value) > 1 && nargin == 1
      sizes = num2cell(double(value));
    else
      sizes{end + 1} = double(value);
    end
    k = k + 1;
  end
end
%=============================================================================
