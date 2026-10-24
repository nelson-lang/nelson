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
    if numel(subs) ~= 1
      error('Nelson:integralInterpolant:invalidQuery', _('Query points must be given as a single numeric array.'));
    end
    xq = subs{1};
    if ~(isnumeric(xq) && isreal(xq))
      error('Nelson:integralInterpolant:invalidQuery', _('Query points must be given as a single numeric array.'));
    end
    result = __integral_interpolant__('evaluate', obj.Data, xq);
    if length(s) > 1
      result = subsref(result, s(2:end));
    end
    varargout{1} = result;
    return
  end
  [varargout{1:nargout}] = builtin('subsref', obj, s);
end
%=============================================================================
