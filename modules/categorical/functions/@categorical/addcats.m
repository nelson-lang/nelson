%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = addcats(A, newCategories, varargin)
  narginchk(2, 4);
  if ~isempty(varargin) && isstruct(varargin{end}) && isscalar(varargin{end})
    varargin = [varargin(1:end - 1), namedargs2cell(varargin{end})];
  end
  requestedNames = categorical.checkCategoryNames(newCategories, false);
  names = {};
  for k = 1:length(requestedNames)
    if ~any(strcmp(A.categoryNames, requestedNames{k}))
      names{end + 1, 1} = requestedNames{k};
    end
  end
  if isempty(varargin)
    if A.isOrdinal
      error('Nelson:addcats:beforeAfterRequired', 'Before or After must be specified for ordinal categorical arrays.');
    end
    A.categoryNames = [A.categoryNames(:); names(:)];
    return
  end
  if length(varargin) ~= 2
    error('Nelson:addcats:invalidOption', 'Invalid %s option.', 'addcats');
  end
  option = char(varargin{1});
  anchor = char(varargin{2});
  idx = find(strcmp(A.categoryNames, anchor), 1);
  if isempty(idx)
    error('Nelson:categorical:categoryNotFound', 'Category not found.');
  end
  if strcmpi(option, 'Before')
    pos = idx - 1;
  elseif strcmpi(option, 'After')
    pos = idx;
  else
    error('Nelson:addcats:invalidOption', 'Invalid %s option.', 'addcats');
  end
  A.categoryNames = [A.categoryNames(1:pos); names(:); A.categoryNames(pos + 1:end)];
  shift = A.codes > pos;
  A.codes(shift) = A.codes(shift) + length(names);
end
%=============================================================================
