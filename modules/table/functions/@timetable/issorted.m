%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = issorted(TT, varargin)
  narginchk(1, Inf);
  st = struct(TT);
  values = timeAsDoubleLocal(st.Properties.RowTimes);
  if length(values) <= 1
    tf = true;
    return
  end
  tf = all(values(1:end - 1) <= values(2:end));
  if ~isempty(varargin) && ischar(varargin{1}) && strcmpi(varargin{1}, 'descend')
    tf = all(values(1:end - 1) >= values(2:end));
  end
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
