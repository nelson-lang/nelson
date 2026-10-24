%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = nflow_blocks_write_json(payload, stem)
  p = [tempname(), '.nflow'];
  if ischar(payload)
    txt = payload;
  else
    txt = jsonencode(payload);
  end
  fid = fopen(p, 'w');
  fwrite(fid, txt);
  fclose(fid);
end
%=============================================================================
