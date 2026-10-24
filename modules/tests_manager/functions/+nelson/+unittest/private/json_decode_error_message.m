%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function message = json_decode_error_message(rawMessage, context, isFile)
  raw = char(rawMessage);
  lowerMessage = lower(raw);
  isUtf8Error = contains(lowerMessage, 'utf8') || contains(lowerMessage, 'utf-8');
  if isUtf8Error
    message = utf8_json_error_message(context, isFile);
    return
  end
  message = invalid_json_error_message(raw, context, isFile);
end
%=============================================================================
function message = utf8_json_error_message(context, isFile)
  if isFile && ~isempty(context)
    message = ['Runner JSON file is not valid UTF-8:', ' ', context, '. ', ...
      'stdout, stderr and result JSON files must be UTF-8 encoded.'];
    return
  end
  message = ['Runner JSON payload is not valid UTF-8.', ' ', ...
    'stdout, stderr and result JSON payloads must be UTF-8 encoded.'];
end
%=============================================================================
function message = invalid_json_error_message(raw, context, isFile)
  if isFile && ~isempty(context)
    message = ['Invalid runner JSON file:', ' ', context];
  else
    message = 'Invalid runner JSON payload.';
  end
  if ~isempty(raw)
    message = [message, ' ', _('jsondecode error:'), ' ', raw];
  end
end
%=============================================================================
