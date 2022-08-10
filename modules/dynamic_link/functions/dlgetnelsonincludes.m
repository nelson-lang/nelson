%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnelsonincludes()
  c = {[modulepath('interpreter'), '/src/include']; ...
  [modulepath('types'), '/src/include']; ...
  [modulepath('error_manager'), '/src/include']; ...
  [modulepath('i18n'), '/src/include']; ...
  [modulepath('stream_manager'), '/src/include']; ...
  [modulepath('validators'), '/src/include']};
end
%=============================================================================
