%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = durationFormatText(obj)
  % Display text of each element; NaN stays the text 'NaN' here, string()
  % turns it into <missing>.
  out = durationRenderArray(obj.SecondsValue, obj.FormatValue);
end
%=============================================================================
