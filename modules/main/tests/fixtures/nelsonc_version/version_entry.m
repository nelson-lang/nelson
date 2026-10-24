%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function version_entry(destination, varargin)
  report = struct('deployed', isdeployed(), 'value', sum((1:100) .^ 2), ...
    'arguments', {varargin}, 'directory', pwd(), 'archive', ctfroot());
  filewrite(destination, jsonencode(report));
  pause(0.2);
  disp('VERSION_EXECUTION_OK');
end
