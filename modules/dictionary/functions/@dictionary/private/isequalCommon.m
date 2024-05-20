%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isequalCommon(varargin)
    % compares two dictionaries
    narginchk(2, 1000);
    nargoutchk(0, 1);
    
    dictA = varargin{1};
    typenameA = class(dictA);
    for k = 2:nargin
      dictB = varargin{k};
      typenameB = class(dictB);
      if ~strcmp(typenameA, typenameB)
        varargout{1} = false;
        return
      end
      R = isequalto(dictA.map, dictB.map);
      if ~R
        varargout{1} = R;
        return
      end
      R = isequalto(dictA.allKeys, dictB.allKeys);
      if ~R
        varargout{1} = R;
        return
      end
      R = isequalto(dictA.keyType, dictB.keyType);
      if ~R
        varargout{1} = R;
        return
      end
      R = isequalto(dictA.valueType, dictB.valueType);
      if ~R
        varargout{1} = R;
        return
      end
    end
    varargout{1} = true;
  end
  %=============================================================================