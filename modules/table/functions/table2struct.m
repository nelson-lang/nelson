%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = table2struct(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if nargin == 2
    error('Nelson:table:invalidNumberOfArguments', 'Invalid number of input arguments.');
  end
  toScalar = false;
  if nargin == 3
    second_arg = varargin{2};
    second_arg = convertStringsToChars(varargin{2});
    if (~ischar(second_arg))
      error('Nelson:table:invalidArgument', 'Invalid %s argument.', 'second');
    end
    if (~strcmpi(second_arg, 'toscalar'))
      error('Nelson:table:invalidArgument', 'Invalid %s argument.', 'second');
    end
    third_arg = varargin{3};
    if isscalar(third_arg) && (isdouble(third_arg) || islogical(third_arg))
      toScalar = third_arg == 1;
    else
      error('Nelson:table:invalidArgument', 'Invalid %s argument.', 'third');
    end
  end
  st = struct();
  T = varargin{1};
  if (~istable(T))
    error('Nelson:table:invalidArgument', 'Invalid %s argument.', 'first');
  end
  asStruct = struct(T);
  if (toScalar)
    % The backing data struct already has one field per variable holding the
    % whole column, so it is exactly the scalar-struct result. Copying it
    % field by field read and reallocated the struct per variable (quadratic).
    st = asStruct.data;
    if isempty(fieldnames(st))
      st = struct();
    end
  else
    nRows = height(T);
    names = fieldnames(asStruct.data);
    if nRows == 0
      st = repmat(struct(), 0, 1);
    else
      % Build the struct array in a single struct(...) call. Assigning
      % st(i).(name) in a per-row loop recopies the whole struct array on
      % every element, which is quadratic.
      nCols = numel(names);
      pairs = cell(1, 2 * nCols);
      for j = 1:nCols
        pairs{2 * j - 1} = names{j};
        pairs{2 * j} = columnToCellLocal(asStruct.data.(names{j}), nRows);
      end
      st = reshape(struct(pairs{:}), nRows, 1);
    end
  end
  varargout{1} = st;
end
%=============================================================================
function c = columnToCellLocal(column, nRows)
  % One cell entry per row, each equal to the brace-indexing result T{i, j}
  % (a cell-valued column keeps its 1x1 cell wrapper), so that struct(...)
  % distributes the values across the struct array elements. Building a plain
  % cell this way is linear; assigning st(i).(name) per element was quadratic.
  c = cell(nRows, 1);
  if ndims(column) <= 2
    for i = 1:nRows
      c{i} = column(i, :);
    end
  else
    trailing = repmat({':'}, 1, ndims(column) - 1);
    for i = 1:nRows
      c{i} = column(i, trailing{:});
    end
  end
end
%=============================================================================
