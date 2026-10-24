%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function directory = ctfroot()
  nargoutchk(0, 1);
  directory = __nelsonc_environment__('deployment');
  if isempty(directory)
    error('Nelson:ctfroot:notDeployed', 'The application archive root is available only in a deployed application.');
  end
end
