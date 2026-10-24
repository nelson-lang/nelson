%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = durationSizeValue(secondsValue, requestedOutputs, varargin)
  if requestedOutputs <= 1
    varargout{1} = size(secondsValue, varargin{:});
  else
    [varargout{1:requestedOutputs}] = size(secondsValue, varargin{:});
  end
end
%=============================================================================
