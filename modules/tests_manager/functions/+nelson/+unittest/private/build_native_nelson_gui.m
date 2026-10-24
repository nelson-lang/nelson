%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [executable, processArguments] = build_native_nelson_gui(command, withMPI)
  [executable, processArguments] = build_native_nelson_mode('nelson-gui', command, withMPI); %#ok<NLS0004>
end
