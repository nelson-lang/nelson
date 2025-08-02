%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(obj, name)
  if ~isempty(name) 
    disp([char(10), name, ' = ', char(10)])
  end
  disp('complexObj_disp:')
  disp('real part');
  disp(obj.r);
  disp('imag part');
  disp(obj.i);
end
%=============================================================================
