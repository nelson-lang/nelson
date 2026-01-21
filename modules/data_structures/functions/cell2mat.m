%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cell2mat(varargin)
  % Convert cell array to matrix.
  %   M = cell2mat(C) converts a cell array C into a matrix M.
  %   The contents of the cells of C must be numeric arrays, character arrays,
  %   logical arrays, structures, or strings, and all cells must contain
  %   arrays of the same type. The sizes of the arrays in the cells must be
  %   compatible so that they can be concatenated to form a single array.
  %   If C is empty, cell2mat returns an empty matrix.
  %   If C is a scalar cell containing a numeric array, character array,
  %   logical array, structure, or string, cell2mat returns the contents of
  %   that cell.
  %   Examples:
  %      C = {[1 2; 3 4], [5,6;7,8]; [9,10;11,12], [13,14;15,16]};
  %      M = cell2mat(C)
  %   See also: num2cell, struct2cell, cellfun.
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  C = varargin{1};
  if isempty(C)
    varargout{1} = [];
    return
  end
  
  if ~iscell(C)
    error(_('First input argument must be a cell array.'));
  end
  
  if isscalar(C) && ...
    (isnumeric(C{1}) || ischar(C{1}) || islogical(C{1}) || isstruct(C{1}) || isstring(C{1}))
    varargout{1} = C{1};
    return
  end
  
  checkStructFieldnames(C);
  isCellOrObject = iscell(C{1}) || isobject(C{1});
  if isCellOrObject
    error(_('Cell with cells or object not supported.'));
  end
  firstCellType = class(C{1});
  classCellElements = cellfun('isclass', C, firstCellType);
  if ~all(classCellElements(:))
    error(_('All elements of cell array must have same type.'));
  end
  
  if (ndims(C) == 2)
    varargout{1} = cell2mat2D(C);
  else
    varargout{1} = cell2matND(C);
  end
end
%=============================================================================
function checkStructFieldnames(C) 
  if isstruct(C{1})
    l = length(C);
    names = cell(l, 1);
    for k = 1:l
      names{k} = fieldnames(c{k});
    end
    if ~isequal(names{:})
      error(_('All structures must have same fieldnames'));
    end
  end
end
%=============================================================================
function M = cell2matND(C)
  sizeC = size(C);
  for dimC = (length(sizeC) - 1):-1:1
    tempCell = cell([sizeC(1:dimC), 1]);
    tempCellSize = size(tempCell);
    tempCellSizeLength = length(tempCellSize);
    tmp = {};
    for idx = 1:prod(tempCellSize)
      [tmp{1:tempCellSizeLength}] = ind2sub(tempCellSize, idx);
      if tempCellSizeLength == 2 && tempCellSize(2) == 1
        tmp = {tmp{1}};
      end
      tempCell{tmp{:}} = cat(dimC + 1, C{tmp{:}, :});
    end
    C = tempCell;
  end
  M = cat(1, C{:});
end
%=============================================================================
function M = cell2mat2D(C)
  M = [];
  [m, n] = size(C);
  if (m >= n)
    M = cell(1, n);
    for k = 1:n
      M{k} = cat(1, C{:, k});
    end    
    M = cat(2, M{:});
  else
    M = cell(m, 1);
    for k = 1:m
      M{k} = cat(2, C{k, :});
    end
    M = cat(1, M{:});
  end
end
%=============================================================================
