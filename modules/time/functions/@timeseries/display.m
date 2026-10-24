%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(obj, varargin)
  if nargin == 2
    name = varargin{1};
  else
    name = inputname(1);
  end
  if ~isempty(name)
    disp([name, ' =']);
    disp(' ');
  end
  disp(obj);
end
%=============================================================================
