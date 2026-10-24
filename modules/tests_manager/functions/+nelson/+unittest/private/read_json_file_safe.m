%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [data, status, message] = read_json_file_safe(filename)
  data = [];
  status = false;
  message = '';
  if isempty(filename) || ~isfile(filename)
    return
  end
  try
    data = jsondecode(filename, '-file');
    status = true;
  catch ex
    data = [];
    status = false;
    message = json_decode_error_message(ex.message, filename, true);
  end
end
%=============================================================================
