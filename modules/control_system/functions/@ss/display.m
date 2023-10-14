%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(varargin)
  SS = varargin{1};
  if nargin == 2
    name = varargin{2};
  else
    name = inputname(1);
  end
  currentFormat = format();
  if ~isempty(name)
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp([name, ' ='])
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  displayMatrix(SS.A, 'A', currentFormat);
  displayMatrix(SS.B, 'B', currentFormat);
  displayMatrix(SS.C, 'C', currentFormat);
  displayMatrix(SS.D, 'D', currentFormat);
  displayMatrix(SS.E, 'E', currentFormat);
  
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  
  if (isstatic(SS))
    disp(_('     Static gain.'));
  else
    if isdt(SS)
      disp(sprintf(_('Sample time: %.4f %s'), SS.Ts, SS.TimeUnit)); 
      disp(_('Discrete-time state-space model.'));
    end
    if isct(SS)
      disp(_('Continuous-time state-space model.'));
    end
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
end
%=============================================================================
function displayMatrix(M, name, format)
  if isempty(M)
    return
  end
  if numel(M) > 17
    disp(['   ', name, ' = ', sizeToString(size(M))])
  else
    display(M, ['   ', name])
  end
  
end
%=============================================================================
function str = sizeToString(sz)
  str = sprintf('[%dx%d]', sz(1), sz(2));
end
%=============================================================================
