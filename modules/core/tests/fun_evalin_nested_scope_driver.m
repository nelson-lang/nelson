%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = fun_evalin_nested_scope_driver()
  evalin('base', 'clear(''nelsonEvalinCountTo'', ''nelsonEvalinResult'')');
  assignin('base', 'nelsonEvalinCountTo', @(n) fun_evalin_nested_scope_inner(n));
  try
    evalin('base', 'nelsonEvalinResult = nelsonEvalinCountTo(4);');
    r = evalin('base', 'nelsonEvalinResult');
  catch ex
    evalin('base', 'clear(''nelsonEvalinCountTo'', ''nelsonEvalinResult'')');
    rethrow(ex);
  end
  evalin('base', 'clear(''nelsonEvalinCountTo'', ''nelsonEvalinResult'')');
end
%=============================================================================
