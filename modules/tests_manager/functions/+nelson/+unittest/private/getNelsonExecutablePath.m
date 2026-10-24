%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function executable = getNelsonExecutablePath(exe)
  persistent nelson_bin_path %#ok<NLS0005>
  if isempty(nelson_bin_path)
    nelson_bin_path = modulepath('nelson', 'bin');
  end
  executable = [nelson_bin_path, '/', exe];
  if ispc()
    executable = [executable, '.exe'];
  end
end
