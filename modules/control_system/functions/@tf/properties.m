%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = properties(obj)
  props = {'Numerator'; 'Denominator'; 'Variable'; 'Ts'; 'TimeUnit'; 'UserData'};
  if (nargout == 0)
    currentFormat = format();
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp('Properties for class tf:');
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    for p = props'
      disp(['    ', p{1}]);
    end
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
  else
    varargout{1} = props;
  end
end
%=============================================================================
