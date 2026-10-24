%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = convertContainedStringsToChars(varargin)
  narginchk(1, Inf);
  nargoutchk(0, nargin);
  for k = 1:nargin
    varargout{k} = convertContained(varargin{k});
  end
end
%=============================================================================
function value = convertContained(value)
  if isstring(value)
    value = convertStringsToChars(value);
  elseif iscell(value)
    for k = 1:numel(value)
      value{k} = convertContained(value{k});
    end
  elseif isstruct(value)
    names = fieldnames(value);
    for idx = 1:numel(value)
      for k = 1:numel(names)
        value(idx).(names{k}) = convertContained(value(idx).(names{k}));
      end
    end
  end
end
%=============================================================================
