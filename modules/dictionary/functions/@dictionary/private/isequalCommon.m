%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
    R = isequalto(numEntries(dictA), numEntries(dictB));
    if ~R
      varargout{1} = R;
      return
    end
    for idx = 1:numel(dictA.allKeys)
      key = dictionary.normalizeStoredKey(dictA, dictA.allKeys{idx});
      keyIndex = dictionary.findKeyIndex(dictB, key);
      if keyIndex == 0
        varargout{1} = false;
        return
      end
      valueA = dictionary.valueAt(dictA, idx);
      valueB = dictionary.valueAt(dictB, keyIndex);
      if strcmp(dictA.valueType, 'cell') && iscell(valueA) && isscalar(valueA)
        valueA = valueA{1};
      end
      if strcmp(dictB.valueType, 'cell') && iscell(valueB) && isscalar(valueB)
        valueB = valueB{1};
      end
      R = isequalto(valueA, valueB);
      if ~R
        varargout{1} = R;
        return
      end
    end
  end
  varargout{1} = true;
end
%=============================================================================
