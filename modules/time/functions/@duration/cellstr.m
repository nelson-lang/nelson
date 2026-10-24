%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = cellstr(obj, fmt)
  % Display text of each element, 'NaN' for a NaN duration.
  if nargin > 1
    obj.Format = fmt;
  end
  C = cellstr(durationFormatText(obj));
end
%=============================================================================
