%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = categories(A, varargin)
  narginchk(1, 3);
  if ~isempty(varargin) && isstruct(varargin{end}) && isscalar(varargin{end})
    varargin = [varargin(1:end - 1), namedargs2cell(varargin{end})];
  end
  names = A.categoryNames;
  if nargin > 1
    if nargin ~= 3 || ~strcmpi(char(varargin{1}), 'OutputType')
      error('Nelson:categories:invalidOption', 'Invalid %s option.', 'categories');
    end
    outputType = char(varargin{2});
    if strcmpi(outputType, 'string')
      names = string(names);
    elseif strcmpi(outputType, 'categorical')
      names = categorical(names, names, names);
    elseif ~strcmpi(outputType, 'char')
      error('Nelson:categories:invalidOutputType', 'Invalid category output type.');
    end
  end
end
%=============================================================================
