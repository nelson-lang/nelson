%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = methods(obj)
  names = {'getString'; 'message';  'getUnformattedString'; 'string'};  
  if (nargout == 0)
    currentFormat = format();
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp('Methods for class message:');
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    for p = names'
      disp(['    ', p{1}]);
    end
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
  else
    varargout{1} = names;
  end
end
%=============================================================================
