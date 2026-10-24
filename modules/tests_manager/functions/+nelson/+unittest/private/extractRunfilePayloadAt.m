%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function payload = extractRunfilePayloadAt(stdoutText, position, sentinel)
  payload = stdoutText(position + length(sentinel):end);
  eol = strfind(payload, newline);
  if ~isempty(eol)
    payload = payload(1:eol(1) - 1);
  end
end
%=============================================================================
