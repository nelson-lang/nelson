%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isequalto(varargin)
  % compares two tf model
  narginchk(2, 1000);
  nargoutchk(0, 1);
  
  sysA = varargin{1};
  typenameA = class(sysA);
  for k = 2:nargin
    sysB = varargin{k};
    typenameB = class(sysB);
    if ~strcmp(typenameA, typenameB)
      varargout{1} = false;
      return
    end
    if isa(sysA, 'tf') || isa(sysB, 'tf')
      R = isequalto(struct(sysA), struct(sysB));
      if ~R
        varargout{1} = R;
        return
      end
    else
      varargout{1} = false;
      return
    end
  end
  varargout{1} = true;
end
%=============================================================================
