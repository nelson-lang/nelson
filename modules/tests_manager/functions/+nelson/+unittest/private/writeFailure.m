%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function writeFailure(fp, msg, eol)
  if isstruct(msg)
    msg = msg.message;
  end
  if length(msg) == 0
    return
  end
  fprintf(fp, ['      <error type="error" message="', 'test failed', '"><![CDATA[', eol]);
  if iscellstr(msg)
    for m = msg(:)'
      fprintf(fp, [m{1}, eol]);
    end
  else
    fprintf(fp, [msg, eol]);
  end
  fprintf(fp, [']]></error>', eol]);
end
%=============================================================================
