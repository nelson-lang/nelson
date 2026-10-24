%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [payload, stdoutText] = extractRunfilePayload(stdoutText)
  sentinel = '__NELSON_UNITTEST_RESULT__';
  payload = '';
  pos = strfind(stdoutText, sentinel);
  if isempty(pos)
    return
  end
  startPos = pos(end);
  payload = stdoutText(startPos + length(sentinel):end);
  eol = strfind(payload, newline);
  if ~isempty(eol)
    payload = payload(1:eol(1) - 1);
  end
  stdoutText = stdoutText(1:startPos - 1);
end
