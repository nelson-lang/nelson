%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = sortrows(varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  A = varargin{1};
  ncols = size(A, 2);
  col = 1:ncols;
  colGiven = false;
  direction = [];
  for a = 2:nargin
    arg = varargin{a};
    if ischar(arg) || isstring(arg) || iscellstr(arg)
      direction = arg;
    elseif isnumeric(arg) || islogical(arg)
      if ~isvector(arg) && ~isempty(arg)
        error('Nelson:elementary_functions:secondInputVectorColumnIndices', 'Second input must be a vector of column indices.');
      end
      col = double(arg(:).');
      colGiven = true;
    else
      error('Nelson:elementary_functions:secondInputVectorColumnIndices', 'Second input must be a vector of column indices.');
    end
  end
  if colGiven && (any(col == 0) || any(col ~= fix(col)) || any(abs(col) > ncols))
    error('Nelson:elementary_functions:columnIndicesNonzeroIntegersExceeding', 'Column indices must be nonzero integers not exceeding the number of columns.');
  end
  % Per-column descending flags: from signed column indices, then overridden
  % by an explicit direction ('ascend'/'descend', scalar or one per column).
  descendFlags = col < 0;
  col = abs(col);
  if ~isempty(direction)
    dirs = cellstr(direction);
    if numel(dirs) == 1
      descendFlags = repmat(strcmpi(dirs{1}, 'descend'), 1, numel(col));
    elseif numel(dirs) == numel(col)
      descendFlags = reshape(strcmpi(dirs, 'descend'), 1, []);
    else
      error('Nelson:elementary_functions:numberDirectionsMatchNumberColumns', 'The number of directions must match the number of columns.');
    end
  end
  m = size(A, 1);
  idx = (1:m).';
  % Stable sort from the least significant key to the most significant key.
  for k = numel(col):-1:1
    c = col(k);
    key = A(idx, c);
    descend = descendFlags(k);
    if iscell(key)
      % 'sort' accepts a single argument only for cell arrays of characters.
      if descend
        % Reverse both the input and output to retain the order of equal keys.
        [~, p] = sort(flipud(key));
        p = numel(key) + 1 - flipud(p(:));
      else
        [~, p] = sort(key);
      end
    elseif descend
      [~, p] = sort(key, 'descend');
    else
      [~, p] = sort(key, 'ascend');
    end
    idx = idx(p);
  end
  varargout{1} = A(idx, :);
  if nargout > 1
    varargout{2} = idx;
  end
end
%=============================================================================
