%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = is_release()
  persistent is_release_version %#ok<NLS0005>
  if isempty(is_release_version)
    ver_comp = version('-compiler');
    is_release_version = startsWith(ver_comp{2}, 'release');
  end
  r = is_release_version;
end
