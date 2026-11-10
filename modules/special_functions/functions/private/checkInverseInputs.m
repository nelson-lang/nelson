%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = checkInverseInputs(varargin)
  % checks floating real inputs of the same size (or scalar) and expands them
  name = varargin{end};
  inputs = varargin(1:end - 1);
  shape = [1, 1];
  proto = zeros(1, 'like', inputs{1});
  for k = 1:numel(inputs)
    v = inputs{k};
    if ~isfloat(v) || ~isreal(v)
      error(['Nelson:', name, ':invalidType'], 'Inputs must be real single or double arrays.');
    end
    proto = proto + zeros(1, 'like', v);
    if ~isscalar(v)
      if ~isequal(shape, [1, 1]) && ~isequal(shape, size(v))
        error(['Nelson:', name, ':sizeMismatch'], 'Inputs must have the same size, or be scalars.');
      end
      shape = size(v);
    end
  end
  first = inputs{1};
  if any(first(:) < 0 | first(:) > 1)
    error(['Nelson:', name, ':invalidProbability'], 'Y must be in the interval [0, 1].');
  end
  varargout = cell(1, numel(inputs));
  for k = 1:numel(inputs)
    varargout{k} = inputs{k} + zeros(shape, 'like', proto);
  end
end
%=============================================================================
