%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __validateattributes__(A, classes, attributes, varargin)
  narginchk(3, 6);
  nargoutchk(0, 0);
  [funcName, varName, argIndex] = localParseContext(varargin{:});
  classes = localTextList(classes, 'classes');
  attributes = localAttributeList(attributes);
  localCheckClasses(A, classes, funcName, varName, argIndex);
  k = 1;
  while k <= numel(attributes)
    name = lower(localAttributeName(attributes{k}));
    switch name
      case {'size', 'numel', 'ncols', 'nrows', 'ndims', '>', '>=', '<', '<='}
        if k == numel(attributes)
          error('Nelson:validators:invalidAttribute', 'Attribute requires a value: %s', name);
        end
        localCheckValuedAttribute(A, name, attributes{k + 1}, funcName, varName, argIndex);
        k = k + 2;
      otherwise
        localCheckAttribute(A, name, funcName, varName, argIndex);
        k = k + 1;
    end
  end
end
%=============================================================================
function [funcName, varName, argIndex] = localParseContext(varargin)
  funcName = '';
  varName = '';
  argIndex = [];
  if nargin == 0
    return
  elseif nargin == 1
    if isnumeric(varargin{1})
      argIndex = localPositiveInteger(varargin{1}, 'argIndex');
    else
      funcName = localTextScalar(varargin{1}, 'funcName');
    end
  elseif nargin == 2
    funcName = localTextScalar(varargin{1}, 'funcName');
    varName = localTextScalar(varargin{2}, 'varName');
  elseif nargin == 3
    funcName = localTextScalar(varargin{1}, 'funcName');
    varName = localTextScalar(varargin{2}, 'varName');
    argIndex = localPositiveInteger(varargin{3}, 'argIndex');
  else
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
%=============================================================================
function value = localTextScalar(value, name)
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ~(ischar(value) && isrow(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
end
%=============================================================================
function values = localTextList(values, name)
  if isstring(values)
    values = cellstr(values);
  end
  if ischar(values)
    values = cellstr(values);
  end
  if ~iscellstr(values)
    error('Nelson:validators:mustBeTextArray', '%s must be a string array or cell array of character vectors.', name);
  end
  values = values(:)';
end
%=============================================================================
function attributes = localAttributeList(attributes)
  if isstring(attributes)
    attributes = cellstr(attributes);
  elseif ischar(attributes)
    attributes = cellstr(attributes);
  end
  if ~iscell(attributes)
    error('Nelson:__validateattributes__:attributesACellArrayString2', 'Attributes must be a cell array or string array.');
  end
  attributes = attributes(:)';
end
%=============================================================================
function name = localAttributeName(value)
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ~(ischar(value) && isrow(value))
    error('Nelson:__validateattributes__:attributeNameText2', 'Attribute name must be text.');
  end
  name = value;
end
%=============================================================================
function value = localPositiveInteger(value, name)
  if ~(isnumeric(value) && isscalar(value) && isfinite(value) && value >= 1 && fix(value) == value)
    error('Nelson:validators:mustBePositiveInteger', '%s must be a positive integer.', name);
  end
  value = double(value);
end
%=============================================================================
function localCheckClasses(A, classes, funcName, varName, argIndex)
  ok = false;
  for k = 1:numel(classes)
    cls = classes{k};
    if strcmpi(cls, 'numeric')
      ok = isnumeric(A);
    elseif strcmpi(cls, 'integer')
      ok = isinteger(A);
    elseif strcmpi(cls, 'float')
      ok = isa(A, 'double') || isa(A, 'single');
    else
      ok = isa(A, cls);
    end
    if ok
      return
    end
  end
  if isempty(funcName)
    error('Nelson:validators:invalidType', ...
      '%s must be one of the following types: %s. Instead its type was %s.', ...
      localSubject(varName, argIndex), localJoin(classes), class(A));
  end
  error(['Nelson:', funcName, ':invalidType'], ...
    '%s must be one of the following types: %s. Instead its type was %s.', ...
    localSubject(varName, argIndex), localJoin(classes), class(A));
end
%=============================================================================
function localCheckValuedAttribute(A, name, value, funcName, varName, argIndex)
  switch name
    case 'size'
      expected = double(value(:)');
      actual = size(A);
      if numel(actual) < numel(expected)
        actual(numel(actual) + 1:numel(expected)) = 1;
      end
      ok = numel(actual) == numel(expected);
      if ok
        for k = 1:numel(expected)
          if ~isnan(expected(k)) && actual(k) ~= expected(k)
            ok = false;
          end
        end
      end
      if ~(ok)
        if isempty(funcName)
          error('Nelson:incorrectSize', '%s must be of the requested size.', localSubject(varName, argIndex));
        end
        error(['Nelson:', funcName, ':incorrectSize'], '%s must be of the requested size.', localSubject(varName, argIndex));
      end
    case 'numel'
      if ~(numel(A) == value)
        if isempty(funcName)
          error('Nelson:incorrectNumel', '%s must have the requested number of elements.', localSubject(varName, argIndex));
        end
        error(['Nelson:', funcName, ':incorrectNumel'], '%s must have the requested number of elements.', localSubject(varName, argIndex));
      end
    case 'ncols'
      if ~(size(A, 2) == value)
        if isempty(funcName)
          error('Nelson:incorrectNumcols', '%s must have the requested number of columns.', localSubject(varName, argIndex));
        end
        error(['Nelson:', funcName, ':incorrectNumcols'], '%s must have the requested number of columns.', localSubject(varName, argIndex));
      end
    case 'nrows'
      if ~(size(A, 1) == value)
        if isempty(funcName)
          error('Nelson:incorrectNumrows', '%s must have the requested number of rows.', localSubject(varName, argIndex));
        end
        error(['Nelson:', funcName, ':incorrectNumrows'], '%s must have the requested number of rows.', localSubject(varName, argIndex));
      end
    case 'ndims'
      if ~(ndims(A) == value)
        if isempty(funcName)
          error('Nelson:incorrectNumdims', '%s must have the requested number of dimensions.', localSubject(varName, argIndex));
        end
        error(['Nelson:', funcName, ':incorrectNumdims'], '%s must have the requested number of dimensions.', localSubject(varName, argIndex));
      end
    case {'>', '>=', '<', '<='}
      localCheckComparison(A, name, value, funcName, varName, argIndex);
  end
end
%=============================================================================
function localCheckAttribute(A, name, funcName, varName, argIndex)
  subject = localSubject(varName, argIndex);
  switch name
    case '2d'
      if ~(ndims(A) == 2)
        if isempty(funcName)
          error('Nelson:expected2D', '%s must be 2-D.', subject);
        end
        error(['Nelson:', funcName, ':expected2D'], '%s must be 2-D.', subject);
      end
    case '3d'
      if ~(ndims(A) <= 3)
        if isempty(funcName)
          error('Nelson:expected3D', '%s must be 3-D or less.', subject);
        end
        error(['Nelson:', funcName, ':expected3D'], '%s must be 3-D or less.', subject);
      end
    case 'column'
      if ~(iscolumn(A))
        if isempty(funcName)
          error('Nelson:expectedColumn', '%s must be a column vector.', subject);
        end
        error(['Nelson:', funcName, ':expectedColumn'], '%s must be a column vector.', subject);
      end
    case 'row'
      if ~(isrow(A))
        if isempty(funcName)
          error('Nelson:expectedRow', '%s must be a row vector.', subject);
        end
        error(['Nelson:', funcName, ':expectedRow'], '%s must be a row vector.', subject);
      end
    case 'scalar'
      if ~(isscalar(A))
        if isempty(funcName)
          error('Nelson:expectedScalar', '%s must be a scalar.', subject);
        end
        error(['Nelson:', funcName, ':expectedScalar'], '%s must be a scalar.', subject);
      end
    case 'scalartext'
      if ~(localIsTextScalar(A))
        if isempty(funcName)
          error('Nelson:expectedScalartext', '%s must be scalar text.', subject);
        end
        error(['Nelson:', funcName, ':expectedScalartext'], '%s must be scalar text.', subject);
      end
    case 'vector'
      if ~(isvector(A))
        if isempty(funcName)
          error('Nelson:expectedVector', '%s must be a vector.', subject);
        end
        error(['Nelson:', funcName, ':expectedVector'], '%s must be a vector.', subject);
      end
    case 'square'
      if ~(ismatrix(A) && size(A, 1) == size(A, 2))
        if isempty(funcName)
          error('Nelson:expectedSquare', '%s must be square.', subject);
        end
        error(['Nelson:', funcName, ':expectedSquare'], '%s must be square.', subject);
      end
    case 'diag'
      if ~(isdiag(A))
        if isempty(funcName)
          error('Nelson:expectedDiag', '%s must be diagonal.', subject);
        end
        error(['Nelson:', funcName, ':expectedDiag'], '%s must be diagonal.', subject);
      end
    case 'nonempty'
      if ~(~isempty(A))
        if isempty(funcName)
          error('Nelson:expectedNonempty', '%s must be nonempty.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonempty'], '%s must be nonempty.', subject);
      end
    case 'nonsparse'
      if ~(~issparse(A))
        if isempty(funcName)
          error('Nelson:expectedNonsparse', '%s must be nonsparse.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonsparse'], '%s must be nonsparse.', subject);
      end
    case 'finite'
      if ~(all(isfinite(A(:))))
        if isempty(funcName)
          error('Nelson:expectedFinite', '%s must be finite.', subject);
        end
        error(['Nelson:', funcName, ':expectedFinite'], '%s must be finite.', subject);
      end
    case 'nonnan'
      if ~(~any(isnan(A(:))))
        if isempty(funcName)
          error('Nelson:expectedNonNaN', '%s must not be NaN.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonNaN'], '%s must not be NaN.', subject);
      end
    case 'binary'
      if ~(localIsRealNumericOrLogical(A) && all(A(:) == 0 | A(:) == 1))
        if isempty(funcName)
          error('Nelson:expectedBinary', '%s must be binary.', subject);
        end
        error(['Nelson:', funcName, ':expectedBinary'], '%s must be binary.', subject);
      end
    case 'even'
      if ~(localIsIntegerValued(A) && all(rem(double(A(:)), 2) == 0))
        if isempty(funcName)
          error('Nelson:expectedEven', '%s must be even.', subject);
        end
        error(['Nelson:', funcName, ':expectedEven'], '%s must be even.', subject);
      end
    case 'odd'
      if ~(localIsIntegerValued(A) && all(rem(double(A(:)), 2) ~= 0))
        if isempty(funcName)
          error('Nelson:expectedOdd', '%s must be odd.', subject);
        end
        error(['Nelson:', funcName, ':expectedOdd'], '%s must be odd.', subject);
      end
    case 'integer'
      if ~(localIsIntegerValued(A))
        if isempty(funcName)
          error('Nelson:expectedInteger', '%s must be an integer.', subject);
        end
        error(['Nelson:', funcName, ':expectedInteger'], '%s must be an integer.', subject);
      end
    case 'real'
      if ~(isreal(A))
        if isempty(funcName)
          error('Nelson:expectedReal', '%s must be real.', subject);
        end
        error(['Nelson:', funcName, ':expectedReal'], '%s must be real.', subject);
      end
    case 'nonnegative'
      if ~(localIsRealNumericOrLogical(A) && all(A(:) >= 0))
        if isempty(funcName)
          error('Nelson:expectedNonnegative', '%s must be nonnegative.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonnegative'], '%s must be nonnegative.', subject);
      end
    case 'nonpositive'
      if ~(localIsRealNumericOrLogical(A) && all(A(:) <= 0))
        if isempty(funcName)
          error('Nelson:expectedNonpositive', '%s must be nonpositive.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonpositive'], '%s must be nonpositive.', subject);
      end
    case 'negative'
      if ~(localIsRealNumericOrLogical(A) && all(A(:) < 0))
        if isempty(funcName)
          error('Nelson:expectedNegative', '%s must be negative.', subject);
        end
        error(['Nelson:', funcName, ':expectedNegative'], '%s must be negative.', subject);
      end
    case 'nonzero'
      if ~(localIsRealNumericOrLogical(A) && ~any(A(:) == 0))
        if isempty(funcName)
          error('Nelson:expectedNonZero', '%s must be nonzero.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonZero'], '%s must be nonzero.', subject);
      end
    case 'positive'
      if ~(localIsRealNumericOrLogical(A) && all(A(:) > 0))
        if isempty(funcName)
          error('Nelson:expectedPositive', '%s must be positive.', subject);
        end
        error(['Nelson:', funcName, ':expectedPositive'], '%s must be positive.', subject);
      end
    case {'decreasing', 'increasing', 'nondecreasing', 'nonincreasing'}
      localCheckMonotonic(A, name, funcName, varName, argIndex);
    otherwise
      error('Nelson:validators:unknownAttribute', 'Unknown attribute: %s', name);
  end
end
%=============================================================================
function localCheckComparison(A, op, value, funcName, varName, argIndex)
  ok = localIsRealNumericOrLogical(A);
  if ok
    switch op
      case '>'
        ok = all(A(:) > value);
      case '>='
        ok = all(A(:) >= value);
      case '<'
        ok = all(A(:) < value);
      case '<='
        ok = all(A(:) <= value);
    end
  end
  if ~(ok)
    subject = localSubject(varName, argIndex);
    switch op
      case '>'
        if isempty(funcName)
          error('Nelson:notGreater', '%s must satisfy the requested range.', subject);
        end
        error(['Nelson:', funcName, ':notGreater'], '%s must satisfy the requested range.', subject);
      case '>='
        if isempty(funcName)
          error('Nelson:notGreaterEqual', '%s must satisfy the requested range.', subject);
        end
        error(['Nelson:', funcName, ':notGreaterEqual'], '%s must satisfy the requested range.', subject);
      case '<'
        if isempty(funcName)
          error('Nelson:notLess', '%s must satisfy the requested range.', subject);
        end
        error(['Nelson:', funcName, ':notLess'], '%s must satisfy the requested range.', subject);
      otherwise
        if isempty(funcName)
          error('Nelson:notLessEqual', '%s must satisfy the requested range.', subject);
        end
        error(['Nelson:', funcName, ':notLessEqual'], '%s must satisfy the requested range.', subject);
    end
  end
end
%=============================================================================
function localCheckMonotonic(A, name, funcName, varName, argIndex)
  ok = localIsRealNumericOrLogical(A) && ~any(isnan(A(:)));
  if ok && size(A, 1) > 1
    D = diff(A, 1, 1);
    switch name
      case 'decreasing'
        ok = all(D(:) < 0);
      case 'increasing'
        ok = all(D(:) > 0);
      case 'nondecreasing'
        ok = all(D(:) >= 0);
      case 'nonincreasing'
        ok = all(D(:) <= 0);
    end
  end
  if ~ok
    subject = localSubject(varName, argIndex);
    switch name
      case 'decreasing'
        if isempty(funcName)
          error('Nelson:expectedDecreasing', '%s must satisfy the requested monotonicity.', subject);
        end
        error(['Nelson:', funcName, ':expectedDecreasing'], '%s must satisfy the requested monotonicity.', subject);
      case 'increasing'
        if isempty(funcName)
          error('Nelson:expectedIncreasing', '%s must satisfy the requested monotonicity.', subject);
        end
        error(['Nelson:', funcName, ':expectedIncreasing'], '%s must satisfy the requested monotonicity.', subject);
      case 'nondecreasing'
        if isempty(funcName)
          error('Nelson:expectedNondecreasing', '%s must satisfy the requested monotonicity.', subject);
        end
        error(['Nelson:', funcName, ':expectedNondecreasing'], '%s must satisfy the requested monotonicity.', subject);
      otherwise
        if isempty(funcName)
          error('Nelson:expectedNonincreasing', '%s must satisfy the requested monotonicity.', subject);
        end
        error(['Nelson:', funcName, ':expectedNonincreasing'], '%s must satisfy the requested monotonicity.', subject);
    end
  end
end
%=============================================================================
function tf = localIsTextScalar(A)
  tf = (ischar(A) && isrow(A)) || (isstring(A) && isscalar(A));
end
%=============================================================================
function tf = localIsRealNumericOrLogical(A)
  tf = (isnumeric(A) || islogical(A)) && isreal(A);
end
%=============================================================================
function tf = localIsIntegerValued(A)
  tf = localIsRealNumericOrLogical(A) && all(isfinite(A(:))) && all(fix(double(A(:))) == double(A(:)));
end
%=============================================================================
function msg = localSubject(varName, argIndex)
  if ~isempty(argIndex) && ~isempty(varName)
    msg = sprintf('input number %d (%s)', argIndex, varName);
  elseif ~isempty(argIndex)
    msg = sprintf('input number %d', argIndex);
  elseif ~isempty(varName)
    msg = sprintf('input %s', varName);
  else
    msg = 'input';
  end
end
%=============================================================================
function txt = localJoin(values)
  txt = '';
  for k = 1:numel(values)
    if k > 1
      txt = [txt, ', '];
    end
    txt = [txt, values{k}];
  end
end
%=============================================================================
