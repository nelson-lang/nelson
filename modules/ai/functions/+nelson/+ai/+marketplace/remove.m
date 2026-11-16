%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Remove an installed assistant skill package by name.
function remove(name)
  narginchk(1, 1);
  nargoutchk(0, 0);
  name = char(name);
  target = [skillsDir(), '/', name];
  if ~isfolder(target)
    error('Nelson:ai:skillNotInstalled', 'No installed skill named "%s".', name);
  end
  rmdir(target, 's');
end
%=============================================================================
