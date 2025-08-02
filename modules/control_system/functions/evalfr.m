%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [frsp] = evalfr(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  w = varargin{2};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
end
