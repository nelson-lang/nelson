%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function payload = decode_runfile_payload(text)
  try
    payload = jsondecode(text);
    payload = decode_runfile_payload_fields(payload);
  catch ex
    message = json_decode_error_message(ex.message, '', false);
    payload = struct('r', 1, 'msg', message, 'stdout', '', 'timing', 0);
  end
end
%=============================================================================
