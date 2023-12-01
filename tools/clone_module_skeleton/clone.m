%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
DESTINATION = [nelsonroot(), '/module_skeleton'];
if isdir(DESTINATION)
  rmdir(DESTINATION, 's');
end
mkdir(DESTINATION);

URL = 'https://github.com/nelson-lang/module_skeleton.git';
try
  repo('clone', URL, DESTINATION);
  bSuccess = true;
catch
  bSuccess = false;
end
if ~bSuccess
  try
    unix(['git clone ', URL, ' ', DESTINATION]);
    bSuccess = true;
  catch
    bSuccess = false;
  end
end
if isdir([DESTINATION, '/.git'])
  rmdir([DESTINATION, '/.git'], 's');
end
if isdir([DESTINATION, '/.github'])
  rmdir([DESTINATION, '/.github'], 's');
end

exit();
%=============================================================================
