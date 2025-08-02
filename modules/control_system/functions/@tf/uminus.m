%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = uminus(sys)
  numerators = sys.Numerator;
  for k = 1:length(numerators)'
    sys.Numerator{k} = - numerators{k};
  end
end
%=============================================================================
