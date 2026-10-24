%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Arithmetic on a zpk model is carried out on its transfer function form and
% returns a tf model.
function sys = mtimes(sys1, sys2)
  sys = mtimes(tf(sys1), tf(sys2));
end
%=============================================================================
