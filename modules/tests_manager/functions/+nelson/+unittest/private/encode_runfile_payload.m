%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function payload = encode_runfile_payload(payload)
  payload = encode_runfile_payload_field(payload, 'msg');
  payload = encode_runfile_payload_field(payload, 'stdout');
end
%=============================================================================
function payload = encode_runfile_payload_field(payload, fieldname)
  encodedField = [fieldname, '_utf8'];
  if isfield(payload, fieldname)
    payload.(encodedField) = double(unicode2native(payload.(fieldname), 'UTF-8'));
    payload.(fieldname) = '';
  end
end
%=============================================================================
