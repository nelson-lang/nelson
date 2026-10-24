%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function writeCase(fp, t, eol)
  if t.isbench && strcmp(t.status, 'Fail') == false
    t.status = "Pass";
  end
  fprintf(fp, ['    <testcase name="', t.name, '" time="', mat2str(t.time, 2), ...
    '" classname="', t.classname, '">', eol]);
  if strcmp(t.status, 'Fail')
    writeFailure(fp, t.msg, eol);
  elseif strcmp(t.status, 'Skip') || strcmp(t.status, 'Interactive') || strcmp(t.status, 'No display')
    fprintf(fp, ['      <skipped/>', eol]);
  end
  fprintf(fp, ['    </testcase>', eol]);
end
