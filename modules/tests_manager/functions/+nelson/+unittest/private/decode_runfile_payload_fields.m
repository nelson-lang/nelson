%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function payload = decode_runfile_payload_fields(payload)
  payload = decode_runfile_payload_field(payload, 'msg');
  payload = decode_runfile_payload_field(payload, 'stdout');
end
%=============================================================================
function payload = decode_runfile_payload_field(payload, fieldname)
  encodedField = [fieldname, '_utf8'];
  if isfield(payload, encodedField)
    try
      bytes = uint8(payload.(encodedField)(:).');
      payload.(fieldname) = native2unicode(bytes, 'UTF-8');
    catch
      payload.(fieldname) = '';
    end
    payload = rmfield(payload, encodedField);
  end
end
%=============================================================================
