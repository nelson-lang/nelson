%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = timeseriesSizeValue(data, requestedOutputs, varargin)
  if isempty(varargin)
    shape = size(data);
    if requestedOutputs <= 1
      varargout{1} = shape;
    else
      for k = 1:requestedOutputs
        if k <= length(shape)
          varargout{k} = shape(k);
        else
          varargout{k} = 1;
        end
      end
    end
  else
    varargout{1} = size(data, varargin{1});
  end
end
%=============================================================================
