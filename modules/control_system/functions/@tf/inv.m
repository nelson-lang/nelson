%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = inv(sys)
  if ~issiso(sys)
    error(_('SISO LTI model expected.'));
  end
  res = sys;
  if isstatic(sys)
    res.Numerator = 1 / sys.Numerator{1}{1};
  else 
    res.Numerator = sys.Denominator{1};
    res.Denominator = sys.Numerator{1};
  end
end
%=============================================================================
