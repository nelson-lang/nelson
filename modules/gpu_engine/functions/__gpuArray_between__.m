%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, ok] = __gpuArray_between__(A, lower, upper, varargin)
  % lower <= A <= upper on the device for real numeric data; the interval type sets which ends
  % are included; ok is false for what the host must handle (other types, options)
  tf = [];
  ok = false;
  if numel(varargin) > 1 || ~(isnumeric(A) || islogical(A)) || ~(isnumeric(lower) || islogical(lower)) ...
      || ~(isnumeric(upper) || islogical(upper)) || ~isreal(A) || ~isreal(lower) || ~isreal(upper)
    return
  end
  intervalType = 'closed';
  if numel(varargin) == 1
    if ~(ischar(varargin{1}) || isstring(varargin{1}))
      return
    end
    intervalType = lower_text(varargin{1});
  end
  switch intervalType
    case 'closed'
      tf = (A >= lower) & (A <= upper);
    case 'open'
      tf = (A > lower) & (A < upper);
    case {'openleft', 'closedright'}
      tf = (A > lower) & (A <= upper);
    case {'openright', 'closedleft'}
      tf = (A >= lower) & (A < upper);
    otherwise
      return
  end
  if ~isgpuarray(tf)
    tf = gpuArray(tf);
  end
  ok = true;
end
%=============================================================================
function t = lower_text(value)
  t = lower(char(value));
end
%=============================================================================
