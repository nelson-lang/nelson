%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(obj, s)
  if strcmp(s(1).type, '()')
    subs = s(1).subs;
    result = evaluate(obj, subs{:});
    if length(s) > 1
      result = subsref(result, s(2:end));
    end
    varargout{1} = result;
    return
  end
  if strcmp(s(1).type, '.')
    state = builtin('struct', obj);
    value = state.(s(1).subs);
    if length(s) > 1
      value = subsref(value, s(2:end));
    end
    varargout{1} = value;
    return
  end
  error('Nelson:geometry:invalidScatteredinterpolantIndexing', 'Invalid scatteredInterpolant indexing.');
end
%=============================================================================
