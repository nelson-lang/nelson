%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [result, status] = read_runfile_output_file(filename)
  [result, status, message] = read_json_file_safe(filename); %#ok<NLS0004>
  if status
    result = decode_runfile_payload_fields(result);
  end
  if ~status
    if isempty(message)
      message = 'Invalid runner result file.';
    end
    result = struct('r', 1, 'msg', message, 'stdout', '', 'timing', 0);
  end
end
%=============================================================================
