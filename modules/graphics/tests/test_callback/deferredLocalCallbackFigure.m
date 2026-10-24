%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function f = deferredLocalCallbackFigure(kind)
  % Builds a figure whose DeleteFcn is a handle to a local function of this
  % file. Callers drop this file from memory before the callback fires, so the
  % handle must keep the definition alive on its own.
  f = figure('Visible', 'off');
  switch kind
    case 'direct'
      set(f, 'DeleteFcn', @recordDeferredDelete);
    case 'anonymous'
      set(f, 'DeleteFcn', @(src, event) recordDeferredDelete(src, event));
    otherwise
      error('Nelson:deferredLocalCallbackFigure:kind', 'Unexpected kind.');
  end
end
%=============================================================================
function recordDeferredDelete(src, event)
  global deferredLocalCallbackLog;
  deferredLocalCallbackLog{end + 1} = ['delete:' src.BeingDeleted ':' event.EventName];
end
%=============================================================================
