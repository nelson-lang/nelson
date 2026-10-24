%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function payloads = extractRunfilePayloads(stdoutText)
  sentinel = '__NELSON_UNITTEST_RESULT__';
  payloads = struct([]);
  positions = strfind(stdoutText, sentinel);
  for k = 1:length(positions)
    payload = extractRunfilePayloadAt(stdoutText, positions(k), sentinel);
    if ~isempty(payload)
      payloads = [payloads; decode_runfile_payload(payload)];
    end
  end
end
%=============================================================================
