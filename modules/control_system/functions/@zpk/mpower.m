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
function sys = mpower(sys1, n)
  sys = mpower(tf(sys1), n);
end
%=============================================================================
