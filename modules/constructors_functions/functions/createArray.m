%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = createArray(varargin)
  % array of a given size filled with a value, with the class of a name, a prototype or the fill value
  dims = {};
  className = '';
  prototype = [];
  hasPrototype = false;
  fill = [];
  hasFill = false;
  k = 1;
  while k <= nargin
    arg = varargin{k};
    if ischar(arg) || (isstring(arg) && isscalar(arg))
      word = char(arg);
      if strcmpi(word, 'like') && k < nargin
        prototype = varargin{k + 1};
        hasPrototype = true;
        k = k + 2;
      elseif strcmpi(word, 'FillValue') && k < nargin
        fill = varargin{k + 1};
        hasFill = true;
        k = k + 2;
      elseif isempty(className) && ~hasPrototype
        className = word;
        k = k + 1;
      else
        error('Nelson:createArray:invalidOption', 'Options must be a class name, ''like'' or ''FillValue''.');
      end
    elseif isempty(className) && ~hasPrototype && ~hasFill && isnumeric(arg) && isreal(arg)
      dims{end + 1} = double(arg);
      k = k + 1;
    else
      error('Nelson:createArray:invalidOption', 'Options must be a class name, ''like'' or ''FillValue''.');
    end
  end
  sz = arraySize(dims);
  if hasPrototype
    base = prototype;
  elseif ~isempty(className)
    base = classPrototype(className);
  elseif hasFill
    base = fill;
  else
    base = 0;
  end
  if iscell(base)
    if hasFill
      A = repmat({fill}, sz);
    else
      A = repmat({[]}, sz);
    end
  elseif isstring(base)
    if hasFill
      A = repmat(string(fill), sz);
    else
      A = repmat(string(missing), sz);
    end
  elseif ischar(base)
    if hasFill
      A = repmat(char(fill), sz);
    else
      A = repmat(char(0), sz);
    end
  elseif islogical(base)
    A = false(sz);
    if hasFill
      A(:) = logical(fill);
    end
  elseif isnumeric(base)
    A = zeros(sz, 'like', base);
    if hasFill
      A = A + fill;
    end
  else
    error('Nelson:createArray:unsupportedClass', 'Class ''%s'' is not supported.', class(base));
  end
end
%=============================================================================
function sz = arraySize(dims)
  % size vector from the dimension arguments; negative sizes become zero
  if isempty(dims)
    sz = [1, 1];
  elseif isscalar(dims) && isscalar(dims{1})
    sz = [dims{1}, dims{1}];
  elseif isscalar(dims)
    sz = dims{1}(:).';
  else
    sz = cellfun(@(v) v, dims);
  end
  sz = max(sz, 0);
end
%=============================================================================
function p = classPrototype(name)
  % scalar of a supported class
  switch name
    case {'double', 'single', 'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64'}
      p = zeros(1, name);
    case 'logical'
      p = false;
    case 'char'
      p = char(0);
    case 'string'
      p = string(missing);
    case 'cell'
      p = {};
    otherwise
      error('Nelson:createArray:unsupportedClass', 'Class ''%s'' is not supported.', name);
  end
end
%=============================================================================
