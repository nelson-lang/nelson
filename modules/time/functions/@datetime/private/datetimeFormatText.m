%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = datetimeFormatText(obj, fmt)
  % Display text of each element; not-a-time stays the text 'NaT' here,
  % string() turns it into <missing>.
  serial = datenum(obj);
  if nargin < 2
    fmt = datetime.resolveDisplayFormat(obj.FormatValue, serial);
  else
    fmt = char(fmt);
  end
  out = datetimeRenderArray(serial, fmt);
end
%=============================================================================
