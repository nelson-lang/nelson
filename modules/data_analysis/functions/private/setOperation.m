%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = setOperation(op, A, B, varargin)
  if nargin > 3
    error(_('Unsupported option.'));
  end
  a = A(:);
  b = B(:);
  keysA = valuesToKeys(a);
  keysB = valuesToKeys(b);
  switch op
    case 'union'
      C = unique([a; b]);
      ia = indicesForValues(keysA, valuesToKeys(C));
      ib = indicesForValues(keysB, valuesToKeys(C));
      values = {reshapeLikeInputs(C, A, B), ia, ib};
    case 'intersect'
      [tf, ~] = ismemberKeys(keysA, keysB);
      ia = find(tf);
      if ~isempty(ia)
        [~, keep] = unique(keysA(ia));
        ia = ia(keep);
      end
      C = unique(a(ia));
      ia = indicesForValues(keysA, valuesToKeys(C));
      ib = indicesForValues(keysB, valuesToKeys(C));
      values = {reshapeLikeInputs(C, A, B), ia, ib};
    case 'setdiff'
      [tf, ~] = ismemberKeys(keysA, keysB);
      ia = find(~tf);
      if ~isempty(ia)
        [~, keep] = unique(keysA(ia));
        ia = ia(keep);
      end
      C = unique(a(ia));
      ia = indicesForValues(keysA, valuesToKeys(C));
      values = {reshapeLikeInputs(C, A, B), ia};
    case 'setxor'
      [tfA, ~] = ismemberKeys(keysA, keysB);
      [tfB, ~] = ismemberKeys(keysB, keysA);
      C = unique([a(~tfA); b(~tfB)]);
      ia = indicesForValues(keysA, valuesToKeys(C));
      ib = indicesForValues(keysB, valuesToKeys(C));
      values = {reshapeLikeInputs(C, A, B), ia, ib};
    otherwise
      error(_('Unsupported set operation.'));
  end
  varargout = values(1:nargout);
end
%=============================================================================
function C = reshapeLikeInputs(C, A, B)
  if isrow(A) && isrow(B)
    C = C(:).';
  else
    C = C(:);
  end
end
%=============================================================================
function keys = valuesToKeys(values)
  keys = cell(numel(values), 1);
  for k = 1:numel(values)
    keys{k} = valueKey(values(k));
  end
end
%=============================================================================
function key = valueKey(value)
  if iscell(value)
    if isempty(value)
      key = 'cell:[]';
    else
      key = ['cell:', valueKey(value{1})];
    end
  elseif isstring(value)
    key = ['string:', char(value)];
  elseif ischar(value)
    key = ['char:', value];
  elseif isnumeric(value) || islogical(value)
    key = [class(value), ':', mat2str(value)];
  else
    key = [class(value), ':', char(string(value))];
  end
end
%=============================================================================
function [tf, loc] = ismemberKeys(keysA, keysB)
  tf = false(length(keysA), 1);
  loc = zeros(length(keysA), 1);
  for k = 1:length(keysA)
    found = find(strcmp(keysB, keysA{k}), 1);
    if ~isempty(found)
      tf(k) = true;
      loc(k) = found;
    end
  end
end
%=============================================================================
function indices = indicesForValues(sourceKeys, wantedKeys)
  indices = zeros(length(wantedKeys), 1);
  for k = 1:length(wantedKeys)
    found = find(strcmp(sourceKeys, wantedKeys{k}), 1);
    if ~isempty(found)
      indices(k) = found;
    end
  end
end
%=============================================================================
