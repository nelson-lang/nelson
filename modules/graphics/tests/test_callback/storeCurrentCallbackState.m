%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function storeCurrentCallbackState(src, event)
  global callbackSource;
  global callbackObject;
  global callbackFigure;
  global callbackFigureFromGcbf;
  global callbackFigureType;
  global callbackFigureFromGcbfType;
  callbackSource = src;
  [callbackObject, callbackFigure] = gcbo();
  callbackFigureFromGcbf = gcbf();
  if isempty(callbackFigure)
    callbackFigureType = '';
  else
    callbackFigureType = callbackFigure.Type;
  end
  if isempty(callbackFigureFromGcbf)
    callbackFigureFromGcbfType = '';
  else
    callbackFigureFromGcbfType = callbackFigureFromGcbf.Type;
  end
end
%=============================================================================
