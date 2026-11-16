%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% List the assistant skill packages installed in the user directory. Returns a
% struct array with fields name and path.
function result = installed()
  narginchk(0, 0);
  nargoutchk(0, 1);
  result = struct('name', {}, 'path', {});
  d = skillsDir();
  entries = dir(d);
  for k = 1:numel(entries)
    nm = entries(k).name;
    if strcmp(nm, '.') || strcmp(nm, '..') || ~entries(k).isdir
      continue
    end
    result(end + 1) = struct('name', nm, 'path', [d, '/', nm]); %#ok<AGROW>
  end
end
%=============================================================================
