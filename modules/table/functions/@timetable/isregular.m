%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isregular(TT, varargin)
  narginchk(1, 2);
  st = struct(TT);
  values = timeAsDoubleLocal(st.Properties.RowTimes);
  if length(values) <= 2
    tf = true;
    return
  end
  steps = diff(values);
  tf = all(abs(steps - steps(1)) <= eps(max(abs(steps(1)), 1)) * 16);
end
%=============================================================================
function values = timeAsDoubleLocal(value)
  if isa(value, 'datetime')
    values = datenum(value);
  elseif isa(value, 'duration')
    values = seconds(value);
  else
    values = value;
  end
  values = values(:);
end
%=============================================================================
